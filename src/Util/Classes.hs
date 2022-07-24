{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Util.Classes where
import           Extras.Conversion (Into (into))


class Nameable a where
    name :: a -> String

instance Nameable a => Into a String where
    into = name

class Empty a where
    empty :: a

-- instance Monoid a => Empty a where
--     empty = mempty
