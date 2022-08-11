{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module Variants.Operand (VOperand, Variant(..)) where

import           LLVM.AST.Operand (Operand)

data Variant = Function | Frozen | MutableMemory

-- An Operand of a certain variant.
-- Since Operand has no type, this serves
-- to keep track of what you can do with it
-- on an IR generation level
newtype VOperand :: Variant -> * where
  VOperand :: Operand -> VOperand a

functionOperand :: Operand -> VOperand Function
functionOperand = VOperand

frozenOperand :: Operand -> VOperand Frozen
frozenOperand = VOperand

mutableMemoryOperand :: Operand -> VOperand MutableMemory
mutableMemoryOperand = VOperand

