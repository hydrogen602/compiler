module ASM.CodeGen where

import           Control.Monad.Trans.Writer.Strict
import qualified Data.Map.Strict                   as Map

import           ASM.Types
import           CodeGen.Classes                   (CodeGeneratable (..))
import           Util.Classes                      (Nameable (name))
import           Util.Literals                     (ConstValue (..))


generateCode :: ASMProgram -> Writer [String] ()
generateCode (ASMProgram (ASMData named unnamed) funcs) = do

  tell ["", "        .data"]

  let
    toRepr :: ConstValue -> String
    toRepr (ConstValueStr s) = '"':s ++ "\""

  Map.traverseWithKey (
    --str_1:  .asciiz     ") = "
    \val label -> tell [name label ++ ": .asciiz " ++ toRepr val]
    ) unnamed



  pure ()

instance CodeGeneratable ASMProgram where
  targetName _ = "mips32-intermediate"
  fileEnding _ = "s"
  generate = execWriter . generateCode
