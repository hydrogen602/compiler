{-# LANGUAGE FlexibleInstances #-}

module Core.Literals (ToLiteral(..), Literals2(..), ConstValue(..), Consts(..), ConstName(..), addLiteral2, singletonLiteral2) where

import qualified Data.Map     as Map
import qualified Data.Set     as Set

import           Core.Classes (Empty (..), Nameable (..))

newtype ConstName = ConstName {getConstName :: String} deriving (Show, Eq, Ord)

newtype ConstValue =
    ConstValueStr String -- string
    deriving (Show, Eq, Ord)

class ToLiteral a where
    toLiteral :: a -> ConstValue

instance ToLiteral String where
    toLiteral = ConstValueStr

data Consts = Consts {
    getNamed    :: Map.Map ConstName ConstValue,
    getLiterals :: Literals2
} deriving (Show, Eq, Ord)

instance Empty Consts where
    empty = Consts mempty empty

instance Nameable ConstName where
  getName = getConstName

newtype Literals2 = Literals2 { getLiterals2 :: Set.Set ConstValue }
    deriving (Show, Eq, Ord)

instance Semigroup Literals2 where
    a <> b = Literals2 $ getLiterals2 a <> getLiterals2 b

instance Monoid Literals2 where
    mempty = Literals2 mempty

instance Empty Literals2 where
    empty = Literals2 mempty

addLiteral2 :: ConstValue -> Literals2 -> Literals2
addLiteral2 value = Literals2 . Set.insert value . getLiterals2

singletonLiteral2 :: ConstValue -> Literals2
singletonLiteral2 = Literals2 . Set.singleton

