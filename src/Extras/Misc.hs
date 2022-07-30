{-# LANGUAGE FunctionalDependencies #-}

module Extras.Misc where

import           Control.Arrow ((&&&))

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust Nothing ma = ma

class Annotated a b c | a -> b c where
  getAnnotation :: a -> b
  getValue :: a -> c
  getAsPair :: a -> (b,c)
  getAsPair = getAnnotation &&& getValue
