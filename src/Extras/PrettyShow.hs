{-# LANGUAGE FlexibleInstances #-}

module Extras.PrettyShow where
import           Data.List                 (intercalate)
import qualified Data.Set                  as Set
import           LLVM.AST.IntegerPredicate (IntegerPredicate (..))
import           Prelude                   hiding (EQ)

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

instance PrettyShow IntegerPredicate where
  pshow EQ  = "equal"
  pshow NE  = "not equal"
  pshow SLT = "less than"
  pshow SGT = "greater than"
  pshow SLE = "less than or equal"
  pshow SGE = "greater than or equal"
  pshow _   = "unsigned comparison" -- FIXME: add info
