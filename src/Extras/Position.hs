module Extras.Position where
import           Extras.PrettyShow (PrettyShow (..))

data Pos = Pos { getLine:: Int, getCol :: Int } deriving (Show, Eq, Ord)

instance PrettyShow Pos where
  pshow (Pos lines cols) = "(Line: " ++ show lines ++ ", Col: " ++ show cols ++ ") "
