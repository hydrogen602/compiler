{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Extras.FixedAnnotated where

import           Control.Arrow ((&&&))

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust Nothing ma = ma

-- For types where the annotation is fixed
class FixedAnnotated a b | a -> b where -- b c | a -> b c where
  getAnnotation :: a c -> b
  getValue :: a c -> c

  toPair :: a c -> (b,c)
  toPair = getAnnotation &&& getValue

  fromPair :: b -> c -> a c
