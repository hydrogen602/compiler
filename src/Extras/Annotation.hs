module Extras.Annotation where

import           Data.Bifunctor    (Bifunctor (..))

import           Extras.PrettyShow (PrettyShow (..))

data Annotated a b =
  Annotated {
    tag   :: a,
    value :: b
  } deriving (Show, Eq, Ord)

instance Functor (Annotated a) where
  fmap f (Annotated a b) = Annotated a (f b)

instance Foldable (Annotated a) where
  foldMap f (Annotated _ b) = f b

instance Traversable (Annotated a) where
  traverse f (Annotated a b) = Annotated a <$> f b

instance Bifunctor Annotated where
  bimap fa fb (Annotated a b) = Annotated (fa a) (fb b)

instance (PrettyShow a, PrettyShow b) => PrettyShow (Annotated a b) where
  pshow (Annotated a b) = pshow a ++ ": " ++ pshow b

instance Monoid a => Applicative (Annotated a) where
  (Annotated m1 f) <*> (Annotated m2 a) = Annotated (m1 <> m2) $ f a
  pure = Annotated mempty

instance Monoid a => Monad (Annotated a) where
  (Annotated m1 a) >>= fm = Annotated (m1 <> m2) b
    where
      (Annotated m2 b) = fm a
