module CodeGen.Classes where

import           Util.Classes (Nameable)

class CodeGeneratable a where
  generate :: a -> [String]
  fileEnding :: a -> String
  targetName :: a -> String
