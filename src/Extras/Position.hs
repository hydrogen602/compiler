module Extras.Position where
import           Extras.PrettyShow (PrettyShow (..))

data Pos = Pos { getLine:: Int, getCol :: Int } deriving (Show, Eq, Ord)

-- data WithPos a = WithPos { pos :: Pos, value :: a }

-- instance Eq a => Eq (WithPos a) where
--   (WithPos _ a) == (WithPos _ b) = a == b

-- instance Ord a => Ord (WithPos a) where
--   compare (WithPos _ a) (WithPos _ b) = compare a b

instance PrettyShow Pos where
  pshow (Pos lines cols) = "(Line: " ++ show lines ++ ", Col: " ++ show cols ++ ") "


-- instance Show a => Show (WithPos a) where
--   show (WithPos (Pos lines cols) val) = "(Line: " ++ show lines ++ ", Col: " ++ show cols ++ ") " ++ show val

-- instance PrettyShow a => PrettyShow (WithPos a) where
--   pshow (WithPos (Pos lines cols) val) = "(Line: " ++ show lines ++ ", Col: " ++ show cols ++ ") " ++ pshow val

-- instance Functor WithPos where
--   fmap f (WithPos pos val) = WithPos pos $ f val

-- instance Foldable WithPos where
--   foldMap f (WithPos _ val) = f val

-- instance Traversable WithPos where
--   traverse f (WithPos pos val) = WithPos pos <$> f val
