{-# LANGUAGE FlexibleContexts #-}

module Extras.Scope where

import qualified Data.Map.Strict           as Map

import           Control.Monad.Error.Class (MonadError)
import           Core.Classes              (Empty (..))
import           Core.CompileResult        (ErrorType (DuplicateNameError, UnknownVariableError),
                                            ResultFailed, ResultT, fromMaybe,
                                            throwError)
import           Core.Types                (LocalVariable)
import           Extras.FixedAnnotated     (firstJust)

data Scope k v = Scope {
  values       :: Map.Map k v,
  parent_scope :: Maybe (Scope k v)
} deriving (Show, Eq, Ord)

instance Ord k => Empty (Scope k v) where
  empty = Scope mempty Nothing

-- (!) :: Ord k => Scope k v -> k -> v
-- scope ! key = case scope !? key of
--   Nothing -> error "Cannot find key"
--   Just sc -> sc

(!?) :: Ord k => Scope k v -> k -> Maybe v
(Scope vals maybe_parent) !? key = firstJust (vals Map.!? key) $ maybe_parent >>= (!? key)

(!!) :: (MonadError ResultFailed m) => Scope LocalVariable v -> LocalVariable -> m v
scope !! key = case scope !? key of
  Nothing -> throwError UnknownVariableError "Cannot find key"
  Just sc -> pure sc

pushScope :: Ord k => Scope k v -> Scope k v
pushScope sc = empty{parent_scope=Just sc}

popScope :: Scope k v -> Maybe (Scope k v)
popScope = parent_scope

newtype DroppedScopes k v = DroppedScopes { getDroppedScopes :: [Map.Map k v] } deriving (Show, Eq, Ord)

instance Foldable (DroppedScopes k) where
  foldMap f = foldMap (foldMap f) . getDroppedScopes
instance Functor (DroppedScopes k) where
  fmap f = DroppedScopes . fmap (fmap f) . getDroppedScopes
instance Traversable (DroppedScopes k) where
  traverse f = (fmap DroppedScopes . traverse (traverse f)) . getDroppedScopes


-- | Collect the scopes from a scope to another scope
-- This is useful for figuring out what variables got dropped
collectDroppedScopes :: (Eq k, Eq v) => Scope k v -> Scope k v -> DroppedScopes k v
collectDroppedScopes scope until_scope
  | scope == until_scope = DroppedScopes []
  | otherwise = DroppedScopes (case parent of
      Nothing -> [map_this_scope]
      Just sc -> map_this_scope:getDroppedScopes (collectDroppedScopes sc until_scope))
  where
    parent = popScope scope
    map_this_scope = values scope

-- with ExceptT error handling system

insertUnique :: (Show k, Ord k, MonadError ResultFailed m) => k -> v -> Scope k v -> m (Scope k v)
insertUnique k v scope
  | k `Map.member` values scope = throwError DuplicateNameError $ show k
  | otherwise = pure $ scope{values=Map.insert k v (values scope)}

lookup :: (Show k, Ord k, Monad m) => k -> Scope k v -> ResultT m v
lookup key scope = fromMaybe (throwError UnknownVariableError $ show key) (scope !? key)
