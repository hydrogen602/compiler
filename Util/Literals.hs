{-# LANGUAGE FlexibleInstances #-}

module Util.Literals (ToLiteral(..), Literals2(..), ConstValue(..), Consts(..), ConstName(..), addLiteral2, singletonLiteral2) where

import qualified Data.Map        as Map
import qualified Data.Set        as Set
import           Numeric.Natural (Natural)

import           Util.Classes    (Empty (..), Nameable (name))
import           Util.Util       (ddot)

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
  name = getConstName

-- getNextFreeConstNum :: Consts -> Int
-- getNextFreeConstNum Consts{getLiterals=mapping} =
--      1 + maybe (-1) fst (listToMaybe $ Map.toDescList mapping)


-- data Literals = LiteralsConstructor {
--     mapping       :: Map.Map ConstValue Natural,
--     current_count :: Natural
--     } deriving (Show, Eq, Ord)

-- instance Empty Literals where
--     empty = LiteralsConstructor mempty 0

-- addLiteral :: ConstValue -> Literals ->  Literals
-- addLiteral value literal@(LiteralsConstructor mapping current_count)
--     | Map.member value mapping = literal
--     | otherwise = LiteralsConstructor (Map.insert value current_count mapping) (current_count + 1)


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

