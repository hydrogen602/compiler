import           Control.Monad.Except   (runExceptT)
import           Control.Monad.Identity (Identity (runIdentity))
import qualified Data.Text.Lazy.IO      as TIO
import           System.Environment     (getArgs)

import           Grammar
import           IRGen.CodeGenIR        (generate)
import           Util.AST               (astToProgram)
import           Util.Util


main :: IO ()
main = do
  args <- getArgs

  -- read input
  s <- case argumentExtract "-i" args of
    (Just inFile) -> readFile inFile
    Nothing       -> getContents

  let -- config output
    outFileName = case argumentExtract "-o" args of
      (Just outFile) -> outFile
      Nothing        -> "out.ll"

    ast = parser (s ++ "\n")
    program = astToProgram ast

    ir = generate program

  TIO.writeFile outFileName ir
