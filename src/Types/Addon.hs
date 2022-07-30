{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Addon where

import qualified LLVM.AST                  as L

import           Control.Monad.Error.Class (MonadError)
import           Data.Foldable             (traverse_)
import           Extras.Misc
import           Extras.PrettyShow         (PrettyShow (pshow))
import           Types.Core
import           Util.CompileResult        (ErrorType (TypeError), ResultFailed,
                                            throwError)


-- data UnknownType = UnknownType deriving (Show, Eq, Ord)

data UnresolvedTyped a = UnresolvedTyped {
  type_'   :: Maybe AType,
  operand' :: a
} deriving (Show, Eq, Ord)

instance Annotated (UnresolvedTyped a) (Maybe AType) a where
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

instance Foldable Typed where
  foldMap f (Typed _ val) = f val

instance Traversable Typed where
  traverse f (Typed type_ val) = Typed type_ <$> f val

typeCheck :: MonadError ResultFailed m => AType -> Typed a -> m (Typed a)
typeCheck aType typed
  | aType == type_ typed = pure typed
  | otherwise = throwTypeError aType $ type_ typed

typeCheck' :: MonadError ResultFailed m => AType -> m (Typed a) -> m (Typed a)
typeCheck' aType typed = do
  ty <- type_ <$> typed
  if aType == ty then
    typed
  else
    throwTypeError aType ty

-- returns the function + the return type
typeCheckFunction :: MonadError ResultFailed m => Typed f -> [Typed a] -> m (Typed f)
typeCheckFunction (Typed (FunctionType args ret) f) params = do
  traverse_ (uncurry typeCheck) $ zip args params
  pure $ Typed ret f

typeCheckFunction (Typed ty _) params = throwError TypeError $ "Expected function, but got " ++ pshow ty

isType :: Typed a -> AType -> Bool
isType (Typed ty _) = (ty ==)

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
