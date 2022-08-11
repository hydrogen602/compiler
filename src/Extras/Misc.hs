module Extras.Misc where

-- Random funcs go here

-- | Only zips lists of the same length
strictZip :: [a] -> [b] -> Maybe [(a,b)]
strictZip [] []             = Just []
strictZip (e1:ls1) (e2:ls2) = ((e1,e2):) <$> strictZip ls1 ls2
strictZip _ _               = Nothing

