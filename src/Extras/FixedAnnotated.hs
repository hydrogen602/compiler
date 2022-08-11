{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Extras.FixedAnnotated where

import           Control.Arrow (second, (&&&))

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

instance {-# OVERLAPPABLE #-} FixedAnnotated a b => Functor a where
  fmap f = uncurry fromPair . second f . toPair

instance {-# OVERLAPPABLE #-} FixedAnnotated a b => Foldable a where
  foldMap f = f . getValue

instance {-# OVERLAPPABLE #-} (Foldable a, Functor a, FixedAnnotated a b) => Traversable a where
  traverse f ta = fromPair annotation <$> value
    where
      (annotation, value) = second f $ toPair ta
