{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Extras.Conversion where
import           Data.String.Transform (ToShortByteString (toShortByteString))
import           LLVM.AST              (Name (..))
import           LLVM.Prelude          (ShortByteString)


class Into a b where
  into :: a -> b

class IntoMaybe a b where
  intoMaybe :: a -> Maybe b

instance (Functor f, Into a b)  => Into (f a) (f b) where
  into = fmap into

instance (Traversable f, IntoMaybe a b)  => IntoMaybe (f a) (f b) where
  intoMaybe = traverse intoMaybe


-- Instances

instance ToShortByteString s => Into s ShortByteString where
  into s = toShortByteString s

instance Into ShortByteString Name where
  into = Name

instance ToShortByteString s => Into s Name where
  into = Name . toShortByteString

