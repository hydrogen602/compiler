{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Addon where

import           Control.Arrow             (Arrow (first))
import           Control.Monad.Error.Class (MonadError)
import           Data.Foldable             (traverse_)

import           Core.CompileResult        (ErrorType (TypeError), ResultFailed,
                                            throwError)
import           Extras.FixedAnnotated
import           Extras.Misc               (strictZip)
import           Extras.PrettyShow         (PrettyShow (pshow))
import           LLVM.AST.Operand          (Operand)
import           Types.Core


-- data UnknownType = UnknownType deriving (Show, Eq, Ord)

data MaybeTyped a = MaybeTyped {
  type_'   :: Maybe AType,
  operand' :: a
} deriving (Show, Eq, Ord, Traversable)

instance FixedAnnotated MaybeTyped (Maybe AType) where
  getAnnotation = type_'
  getValue = operand'
  fromPair = MaybeTyped

data Typed a = Typed {
  type_   :: AType,
  operand :: a
} deriving (Show, Eq, Ord, Traversable)

instance PrettyShow (Typed Operand) where
  pshow (Typed ty _) = "operand: " ++ pshow ty

instance FixedAnnotated Typed AType where
  getAnnotation = type_
  getValue = operand
  fromPair = Typed

toMaybeTyped :: Typed a -> MaybeTyped a
toMaybeTyped = uncurry fromPair . first Just . toPair

typeCheck :: MonadError ResultFailed m => AType -> Typed a -> m (Typed a)
typeCheck aType typed
  | aType == type_ typed = pure typed
  | otherwise = throwTypeError aType $ type_ typed

typeCheck' :: MonadError ResultFailed m => Maybe AType -> m (Typed a) -> m (Typed a)
typeCheck' maType typed = do
  ty <- getAnnotation <$> typed
  case maType of
    Nothing -> typed
    Just aType ->
      if aType == ty then
        typed
      else
        throwTypeError aType ty

-- returns the function + the return type
typeCheckFunction :: MonadError ResultFailed m => Typed f -> [Typed a] -> m (Typed f)
typeCheckFunction (Typed (FunctionType args ret) f) parameters = do
  case strictZip args parameters of
    Just zipped -> traverse_ (uncurry typeCheck) zipped
    Nothing     -> throwError TypeError "Function argument count mismatch"

  pure $ Typed ret f
typeCheckFunction (Typed ty _) _ = throwError TypeError $ "Expected function, but got " ++ pshow ty


isCompatibleType :: MaybeTyped a -> AType -> Bool
isCompatibleType (MaybeTyped Nothing _)   = const True
isCompatibleType (MaybeTyped (Just ty) _) = (ty ==)


isType :: Typed a -> AType -> Bool
isType (Typed ty _) = (ty ==)
