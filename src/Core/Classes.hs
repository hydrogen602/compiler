{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Classes where
import           Extras.Conversion (Into (into))

class Nameable a where
    getName :: a -> String

instance Nameable a => Into a String where
    into = getName

class Empty a where
    empty :: a
