module CodeGen.Generator where
import           CodeGen.Classes (CodeGeneratable (..))
import           Data.Maybe      (fromMaybe)

writeToFile :: CodeGeneratable a => a -> Maybe FilePath -> IO ()
writeToFile code maybePath = writeFile path text
  where
    text = unlines $ generate code
    path = fromMaybe ("out." ++ fileEnding code) maybePath

writeToStdout :: CodeGeneratable a => a -> IO ()
writeToStdout code = putStrLn text
  where
    text = unlines $ generate code
