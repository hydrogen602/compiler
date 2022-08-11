{-# LANGUAGE FlexibleInstances #-}

module Extras.PrettyShow where
import           Data.List (intercalate)
import qualified Data.Set  as Set

class PrettyShow a where
  pshow :: a -> String


instance PrettyShow Int where
  pshow = show
instance PrettyShow Integer where
  pshow = show
instance PrettyShow String where
  pshow = id
instance PrettyShow a => PrettyShow [a] where
  pshow = ('[':) . (++"]") . (intercalate ", " . map pshow)
instance PrettyShow a => PrettyShow (Maybe a) where
  pshow = maybe "" pshow
instance PrettyShow a => PrettyShow (Set.Set a) where
  pshow = ('{':) . (++"}") . (intercalate ", " . map pshow) . Set.toList

