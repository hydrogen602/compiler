{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Types.Core where

import           Control.Arrow             ((&&&))
import qualified Data.Map.Strict           as Map
import           Data.String               (IsString (fromString))
import qualified LLVM.AST.Type             as T

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Identity    (Identity (runIdentity))
import           Data.List                 (intercalate)
import           Extras.PrettyShow
import           Util.Classes              (Nameable (..))
import           Util.CompileResult        (ErrorType (DuplicateTypeError, UnknownTypeError),
                                            ResultFailed, ResultT, fromSuccess,
                                            throwError)
import           Util.Util                 (dddot, ddot)


data AType =
    TypeName { getTypeName :: String }
  | FunctionType {
    arguments  :: [AType],
    returnType :: AType
  }
  deriving (Show, Eq, Ord) -- ToDo: Turn FunctionType into Kinds

instance PrettyShow AType where
  pshow (TypeName n) = n
  pshow (FunctionType args ret) =
    "(" ++ intercalate ", " (map pshow args) ++ ") -> " ++ pshow ret

instance IsString AType where
  fromString = TypeName


getContainedTypes :: AType -> [AType]
getContainedTypes (TypeName _)            = []
getContainedTypes (FunctionType args ret) = args ++ [ret]


-- data ATypeClass = ATypeClass {
--   typeParams :: [AType],
--   classFunctions ::  -- ToDo
-- } deriving (Eq, Ord, Show)

newtype TypeTracker = TypeTracker {
  getTypes :: Map.Map AType T.Type
  -- getTypeClasses ::
  } deriving (Eq, Ord, Show)

instance PrettyShow TypeTracker where
  pshow = pshow . Map.keys . getTypes

instance Semigroup TypeTracker where
  (TypeTracker t1) <> (TypeTracker t2) =
    if null (Map.intersection t1 t1) then
      TypeTracker (t1 <> t2)
    else
      error $ "Conflicting definiton for types: " ++ pshow (Map.keys $ Map.intersection t1 t1)

instance Monoid TypeTracker where
  mempty = TypeTracker mempty

-- type tracker helpers

fromList :: [(AType, T.Type)] -> TypeTracker
fromList ls = TypeTracker $ Map.fromList ls

insert :: Monad m => AType -> T.Type -> TypeTracker -> ResultT m TypeTracker
insert name repr tracker
  | Map.member name (getTypes tracker) = throwError DuplicateTypeError $ pshow name
  | otherwise = pure $ TypeTracker $ Map.insert name repr $ getTypes tracker

member :: AType -> TypeTracker -> Bool
member name = Map.member name . getTypes

requireType :: MonadError ResultFailed m => AType -> TypeTracker -> m ()
requireType type_ tracker
  | member type_ tracker = pure ()
  | otherwise = throwError UnknownTypeError $ pshow type_

get :: MonadError ResultFailed m => AType -> TypeTracker -> m T.Type
get name tracker = maybe
  (throwError UnknownTypeError $ pshow name)
  pure maybe_result
  where
    maybe_result = Map.lookup name $ getTypes tracker


-- other stuff

-- ToDo: Kinds - Pointers are * -> * as there can be a pointer of anything

builtinTypes :: TypeTracker
builtinTypes = fromList [
  ("()", T.void),
  ("i32", T.i32),
  ("i64", T.i64),
  ("bool", T.i1),
  ("*i32", T.ptr T.i32),
  ("*i64", T.ptr T.i64),
  ("*bool", T.ptr T.i1)
  ]




