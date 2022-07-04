module ASM.Gen where

import           Control.Monad.Trans.Writer.Strict

import           ASM.Types
import           CodeGen.Classes                   (CodeGeneratable (..))


generateCode :: ASMProgram -> Writer [String] ()
generateCode (ASMProgram asmData funcs) = undefined

instance CodeGeneratable ASMProgram where
  targetName _ = "MIPS-32"
  fileEnding _ = "s"
  generate = execWriter . generateCode
