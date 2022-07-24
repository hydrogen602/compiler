module Extras.Misc where

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust Nothing ma = ma
