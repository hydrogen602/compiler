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
  inFile  :: String,
  outFile :: String,
  irOnly  :: Bool,
  llcPath :: String
  }

options :: Parser Options
options = Options
  <$> strOption (long "input" <> short 'i' <> metavar "INPUT_FILE" <> help "File to compile" )
  <*> strOption (long "output" <> short 'o' <> value "" <> metavar "OUTPUT_FILE" <> help "Output file" )
  <*> switch (short 'c' <> help "Emit LLVM IR")
  <*> strOption (long "llc-path" <> value "llc" <> metavar "LLC_PATH" <> help "Path to llc")

main :: IO ()
main = execParser with_info >>= mainOpts
  where
    with_info = info (options <**> helper)
      ( fullDesc
      <> progDesc "Compile a file into LLVM IR"
      <> header "Compiler" )

mainOpts :: Options -> IO ()
mainOpts (Options inFile rawOutFile irOnly llcPath) = do
  let outFile = case (rawOutFile, irOnly) of
        ("", True)  -> "out.ll"
        ("", False) -> "out.o"
        (s, _)      -> s

  fileContent <- (++"\n") <$> readFile inFile
  let -- config output
    ast = parser fileContent
    program = astToProgram inFile ast
    !ir = generate program  -- throw errors out here?

  if irOnly then
    TIO.writeFile outFile ir
  else
    compileIR llcPath ir outFile

compileIR :: FilePath -> T.Text -> FilePath -> IO ()
compileIR llcPath text file = do
  let pConfig = (proc llcPath ["-o", file, "-filetype=obj"]){std_in=CreatePipe}

  (stdin, stdout, stderr, process) <- createProcess pConfig
  case stdin of
    Nothing -> error "Created Process didn't return a stdin handle"
    Just h -> do
      TIO.hPutStrLn h text
      hClose h

  result <- waitForProcess process
  case result of
    ExitSuccess   -> pure ()
    ExitFailure n -> error "llc failed to compile IR to object file"
