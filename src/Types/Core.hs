{-# LANGUAGE OverloadedStrings #-}

module Types.Core where

import qualified Data.Set           as Set
import           Data.String        (IsString (fromString))
import qualified LLVM.AST.Type      as T

import           Extras.PrettyShow
import           Util.Classes       (Nameable (..))
import           Util.CompileResult (ErrorType (DuplicateTypeError), ResultT,
                                     throwError)

newtype TypeName = TypeName { getTypeName :: String } deriving (Show, Eq, Ord)

instance PrettyShow TypeName where
  pshow = getTypeName
instance Nameable TypeName where
  getName = getTypeName
instance IsString TypeName where
  fromString = TypeName


data AType = AType {
  getType        :: TypeName,
  representation :: T.Type
  } deriving (Eq, Ord, Show)

instance PrettyShow AType where
  pshow = getTypeName . getType

-- data ATypeClass = ATypeClass {
--   typeParams :: [AType],
--   classFunctions ::  -- ToDo
-- } deriving (Eq, Ord, Show)

newtype TypeTracker = TypeTracker {
  getTypes :: Set.Set AType
  -- getTypeClasses ::
  } deriving (Eq, Ord, Show)

instance PrettyShow TypeTracker where
  pshow = pshow . getTypes

instance Semigroup TypeTracker where
  (TypeTracker t1) <> (TypeTracker t2) =
    if null (Set.intersection t1 t1) then
      TypeTracker (t1 <> t2)
    else
      error $ "Conflicting definiton for types: " ++ pshow (Set.intersection t1 t1)

instance Monoid TypeTracker where
  mempty = TypeTracker mempty

-- type tracker helpers

insert :: Monad m => AType -> TypeTracker -> ResultT m TypeTracker
insert type_ tracker
  | Set.member type_ (getTypes tracker) = throwError DuplicateTypeError $ pshow type_
  | otherwise = pure $ TypeTracker $ Set.insert type_ $ getTypes tracker

member :: AType -> TypeTracker -> Bool
member type_ = Set.member type_ . getTypes

-- other stuff

-- ToDo: Kinds - Pointers are * -> * as there can be a pointer of anything

builtinTypes :: TypeTracker
builtinTypes = TypeTracker $ Set.fromList [
  AType "i32" T.i32,
  AType "i64" T.i64,
  AType "bool" T.i1,
  AType "*i32" $ T.ptr T.i32,
  AType "*i64" $ T.ptr T.i64,
  AType "*bool" $ T.ptr T.i1
  ]




