{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Addon where

import           Control.Arrow         (Arrow (first))

import           Extras.FixedAnnotated
import           Extras.PrettyShow     (PrettyShow (pshow))
import           LLVM.AST.Operand      (Operand)
import           Types.Core


data MaybeTyped a = MaybeTyped {
  type_'   :: Maybe AType,
  operand' :: a
} deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance PrettyShow (MaybeTyped Operand) where
  pshow (MaybeTyped (Just ty) _) = "operand: " ++ pshow ty
  pshow (MaybeTyped Nothing _)   = "operand: ?"


instance FixedAnnotated MaybeTyped (Maybe AType) where
  getAnnotation = type_'
  getValue = operand'
  fromPair = MaybeTyped

data Typed a = Typed {
  type_   :: AType,
  operand :: a
} deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance PrettyShow (Typed Operand) where
  pshow (Typed ty _) = "operand: " ++ pshow ty

instance FixedAnnotated Typed AType where
  getAnnotation = type_
  getValue = operand
  fromPair = Typed

toMaybeTyped :: Typed a -> MaybeTyped a
toMaybeTyped = uncurry fromPair . first Just . toPair
