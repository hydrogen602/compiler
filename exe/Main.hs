{-# LANGUAGE TupleSections #-}

import           Control.Monad.Except   (runExceptT)
import           Control.Monad.Identity (Identity (runIdentity))
import qualified Data.Text.Lazy.IO      as TIO
import           System.Environment     (getArgs)

import           Grammar
import           IRGen.CodeGenIR        (generate)
import           Core.AST               (astToProgram)
import           Core.Util


main :: IO ()
main = do
  args <- getArgs

  -- read input
  (fileContent, inFile) <- case argumentExtract "-i" args of
    (Just inFile) -> (,inFile::FilePath) <$> readFile inFile
    Nothing       -> error "No input specified"

  let -- config output
    outFileName = case argumentExtract "-o" args of
      (Just outFile) -> outFile
      Nothing        -> "out.ll"

    ast = parser (fileContent ++ "\n")
    program = astToProgram inFile ast

    ir = generate program

  TIO.writeFile outFileName ir
