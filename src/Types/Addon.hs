{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Addon where

import qualified LLVM.AST    as L

import           Extras.Misc
import           Types.Core


data UnknownType = UnknownType deriving (Show, Eq, Ord)

data UnresolvedTypedOperand = UnresolvedTypedOperand {
  type_'   :: Either UnknownType AType,
  operand' :: L.Operand
} deriving (Show, Eq, Ord)

instance Annotated UnresolvedTypedOperand (Either UnknownType AType) L.Operand where
  getAnnotation = type_'
  getValue = operand'

data Typed a = Typed {
  type_   :: AType,
  operand :: a
} deriving (Show, Eq, Ord)

instance Annotated (Typed a) AType a where
  getAnnotation = type_
  getValue = operand

instance Functor Typed where
  fmap f (Typed t op) = Typed t (f op)

-- data SomeTypedOperand f = TypedOperand {
--   type_   :: f Typed,
--   operand :: L.Operand
-- }



-- type UnresolvedTypedOperand = SomeTypedOperand (Either UnknownType)
-- deriving instance Show UnresolvedTypedOperand
-- deriving instance Ord UnresolvedTypedOperand
-- deriving instance Eq UnresolvedTypedOperand

-- type TypedOperand = SomeTypedOperand Identity
-- deriving instance Show TypedOperand
-- deriving instance Ord TypedOperand
-- deriving instance Eq TypedOperand
