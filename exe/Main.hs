{-# LANGUAGE BangPatterns #-}

import           Control.Monad.Except   (runExceptT)
import           Control.Monad.Identity (Identity (runIdentity))
import qualified Data.Text.Lazy         as T
import qualified Data.Text.Lazy.IO      as TIO
import           Options.Applicative
import           System.Environment     (getArgs)
import           System.Exit            (ExitCode (ExitFailure, ExitSuccess))
import           System.IO              (hClose)
import           System.Process         (CreateProcess (std_in),
                                         StdStream (CreatePipe), createProcess,
                                         proc, waitForProcess)

import           Core.AST               (astToProgram)
import           Core.Util
import           Grammar
import           IRGen.CodeGenIR        (generate)


data Options = Options {
  inFile    :: String,
  outFile   :: String,
  emitIR    :: Bool,
  emitObj   :: Bool,
  clangPath :: String
  }


options :: Parser Options
options = Options
  <$> strOption (long "input" <> short 'i' <> metavar "INPUT_FILE" <> help "File to compile" )
  <*> strOption (long "output" <> short 'o' <> value "" <> metavar "OUTPUT_FILE" <> help "Output file" )
  <*> switch    (long "emit-llvm" <> help "Emit LLVM IR")
  <*> switch    (short 'c' <> help "Emit Object files")
  <*> strOption (long "clang-path" <> value "clang" <> metavar "CLANG_PATH" <> help "Path to clang")

main :: IO ()
main = execParser with_info >>= mainOpts
  where
    with_info = info (options <**> helper)
      ( fullDesc
      <> progDesc "Compile a file into LLVM IR"
      <> header "Compiler" )

data Emit = IR | OBJ | EXE deriving (Show, Eq, Ord)
getEmit :: Options -> Emit
getEmit Options{emitIR=ir, emitObj=obj} = case (ir, obj) of
  (False, False) -> EXE
  (True, False)  -> IR
  (False, True)  -> OBJ
  (True, True)   -> IR -- idk


mainOpts :: Options -> IO ()
mainOpts opts@(Options inFile rawOutFile emitIR emitObj  clangPath) = do
  let
    emitter = getEmit opts
    outFile = case (rawOutFile, emitter) of
      ("", EXE) -> "a.out"
      ("", IR)  -> "out.ll"
      ("", OBJ) -> "out.o"
      (s, _)    -> s

  fileContent <- (++"\n") <$> readFile inFile
  let -- config output
    ast = parser fileContent
    program = astToProgram inFile ast
    !ir = generate program  -- throw errors out here?

  case emitter of
    IR  -> TIO.writeFile outFile ir
    OBJ -> compileToObj clangPath ir outFile
    EXE -> compileToExe clangPath ir outFile

compileToObj :: FilePath -> T.Text -> FilePath -> IO ()
compileToObj clangPath text file =
  runCommandWithInput clangPath text ["-Wno-override-module", "-c", "-o", file, "-x", "ir", "-"]

compileToExe :: FilePath -> T.Text -> FilePath -> IO ()
compileToExe clangPath text file = do
  compileLib
  runCommandWithInput clangPath text ["-Wno-override-module", "-o", file, "libc/libc.a", "-x", "ir", "-"]

runCommandWithInput :: FilePath -> T.Text -> [String] -> IO ()
runCommandWithInput command text args = do
  let pConfig = (proc command args){std_in=CreatePipe}

  (stdin, stdout, stderr, process) <- createProcess pConfig
  case stdin of
    Nothing -> error "Internal Error: Created process didn't return a stdin handle"
    Just h -> do
      TIO.hPutStrLn h text
      hClose h
  result <- waitForProcess process
  case result of
    ExitSuccess   -> pure ()
    ExitFailure n -> error "clang compilation failed"

compileLib :: IO ()
compileLib = do
  let pConfig = proc "make" ["-C", "libc", "libc.a"]

  (stdin, stdout, stderr, process) <- createProcess pConfig
  result <- waitForProcess process
  case result of
    ExitSuccess   -> pure ()
    ExitFailure n -> error "library compilation failed"
