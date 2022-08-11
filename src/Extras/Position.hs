module Extras.Position where
import           Extras.PrettyShow (PrettyShow (..))

data Pos = Pos { getLine:: Int, getCol :: Int } deriving (Show, Eq, Ord)

instance PrettyShow Pos where
  pshow (Pos lns cols) = "(Line: " ++ show lns ++ ", Col: " ++ show cols ++ ") "
