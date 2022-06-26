module Util.Classes where

class Nameable a where
    name :: a -> String

class Empty a where
    empty :: a

-- instance Monoid a => Empty a where
--     empty = mempty
