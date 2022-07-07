module ASM.CodeGen where

import           Control.Monad.Trans.Writer.Strict
import qualified Data.Map.Strict                   as Map

import           ASM.Types
import           CodeGen.Classes                   (CodeGeneratable (..))
import           Data.Foldable                     (traverse_)
import           Util.Classes                      (Nameable (name))
import           Util.Literals                     (ConstValue (..))

oneLine = tell . (:[])
oneLineIndent = tell . (:[]) . ("    "++)

generateFunc :: ASMFunc -> Writer [String] ()
generateFunc (ASMFunc label lines params maybe_out_reg) = do
  oneLine $ name label ++ ":"
  oneLineIndent "addi        $sp, $sp, -4"
  oneLineIndent "sw          $ra, 0($sp)"
  oneLine ""

  oneLineIndent "lw          $ra, 0($sp)"
  oneLineIndent "addi        $sp, $sp, 4"
  oneLineIndent "jr          $ra"
  oneLine ""

generateCode :: ASMProgram -> Writer [String] ()
generateCode (ASMProgram (ASMData named unnamed) funcs) = do

  tell [
    "        .text",
    "        .globl      main",
    ""]

  traverse_ generateFunc funcs

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
