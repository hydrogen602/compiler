module Extras.Misc where
import           Data.Maybe (fromMaybe)


-- Random funcs go here

-- move over here from Core.Util
ddot :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
ddot = (.) (.) (.)

-- | Only zips lists of the same length
strictZip :: [a] -> [b] -> Maybe [(a,b)]
strictZip [] []             = Just []
strictZip (e1:ls1) (e2:ls2) = ((e1,e2):) <$> strictZip ls1 ls2
strictZip _ _               = Nothing

strictZip' :: [a] -> [b] -> [(a,b)]
strictZip' = fromMaybe (error "Internal Error: strictZip'") `ddot` strictZip
