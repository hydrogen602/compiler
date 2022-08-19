{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Core where

import qualified Data.Map.Strict           as Map
import qualified LLVM.AST.Type             as T

import           Control.Monad.Error.Class (MonadError)
import           Core.CompileResult        (ErrorType (TypeError, UnknownTypeError),
                                            ResultFailed, throwError,
                                            withContext)
import           Core.Util                 (quote)
import           Data.List                 (intercalate)
import           Extras.PrettyShow

-- newtype TypeLabel = TypeLabel String deriving (Show, Eq, Ord)
-- instance PrettyShow TypeLabel where
--   pshow (TypeLabel n) = n

-- | Represents a data type together with all its necessary information like if its boxed or not
data AType =
    TypeName { getTypeName' :: String }
  | FunctionType {
    arguments  :: [AType],
    returnType :: AType
  }
  deriving (Show, Eq, Ord)


data Allocation = Stack | Heap | Static deriving (Show, Eq, Ord)

getTypeName :: AType -> Maybe String
getTypeName (TypeName n)       = Just n
getTypeName (FunctionType _ _) = Nothing

throwTypeError :: MonadError ResultFailed m => AType -> AType -> m a
throwTypeError expected actual = throwError TypeError $
  "Expected " ++ quote (pshow expected) ++ " but got " ++ quote (pshow actual)

instance PrettyShow AType where
  pshow (TypeName n) = n
  pshow (FunctionType args ret) =
    "(" ++ intercalate ", " (map pshow args) ++ ") -> " ++ pshow ret


getContainedTypes :: AType -> [AType]
getContainedTypes (TypeName _)            = []
getContainedTypes (FunctionType args ret) = args ++ [ret]


newtype TypeTracker = TypeTracker {
  getTypes :: Map.Map AType (T.Type, Allocation)
  -- getTypeClasses ::
  } deriving (Eq, Ord, Show)

instance PrettyShow TypeTracker where
  pshow = pshow . Map.keys . getTypes

instance Semigroup TypeTracker where
  (TypeTracker t1) <> (TypeTracker t2) =
    if null (Map.intersection t1 t1) then
      TypeTracker (t1 <> t2)
    else
      error $ "Conflicting definition for types: " ++ pshow (Map.keys $ Map.intersection t1 t1)

-- no mempty as TypeTracker should have at least the built-in types, and is thus not empty
-- instance Monoid TypeTracker where
--   mempty = TypeTracker mempty

newTypeTracker :: TypeTracker
newTypeTracker = builtinTypes

-- type tracker helpers

fromList :: [(AType, (T.Type, Allocation))] -> TypeTracker
fromList ls = TypeTracker $ Map.fromList ls

-- insert :: Monad m => AType -> T.Type -> TypeTracker -> ResultT m TypeTracker
-- insert name repr tracker
--   | Map.member name (getTypes tracker) = throwError DuplicateTypeError $ pshow name
--   | otherwise = pure $ TypeTracker $ Map.insert name repr $ getTypes tracker

member :: AType -> TypeTracker -> Bool
member name = Map.member name . getTypes

requireType :: MonadError ResultFailed m => AType -> TypeTracker -> m ()
requireType type_ tracker
  | member type_ tracker = pure ()
  | otherwise = throwError UnknownTypeError $ pshow type_

get :: MonadError ResultFailed m => AType -> TypeTracker -> m (T.Type, Allocation)
get name tracker = maybe
  (withContext ("Available types are: " <> types) $ throwError UnknownTypeError $ pshow name)
  pure maybe_result
  where
    maybe_result = Map.lookup name $ getTypes tracker
    types = intercalate ", " $ map pshow $ Map.keys $ getTypes tracker

-- other stuff

-- ToDo: Kinds - Pointers are * -> * as there can be a pointer of anything

unit :: AType
unit = TypeName "()"
i32 :: AType
i32 = TypeName "i32"
i64 :: AType
i64 = TypeName "i64"
bool :: AType
bool = TypeName "bool"
-- i32Ptr :: AType
-- i32Ptr = TypeName "*i32"
-- i64Ptr :: AType
-- i64Ptr = TypeName "*i64"
-- boolPtr :: AType
-- boolPtr = TypeName "*bool"
arrayList :: AType
arrayList = TypeName "ArrayList"

builtinTypes :: TypeTracker
builtinTypes = fromList [
  (unit, (T.void, Stack)),
  (i32, (T.i32, Stack)),
  (i64, (T.i64, Stack)),
  (bool, (T.i1, Stack)),
  -- (i32Ptr, (T.ptr T.i32, Stack)),
  -- (i64Ptr, (T.ptr T.i64, Stack)),
  -- (boolPtr, (T.ptr T.i1, Stack)),
  (arrayList, (T.ptr $ T.StructureType False [T.i32, T.i32, T.ptr T.i32], Heap))
  ]
