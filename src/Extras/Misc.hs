module Extras.Misc where
import           Data.Maybe  (fromMaybe)
import           Data.Monoid (Alt (Alt, getAlt), Dual (Dual, getDual))


-- Random funcs go here

-- move over here from Core.Util
ddot :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
ddot = (.) (.) (.)

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> (a -> f c)
(<.>)= (.) . fmap

infixr 9 <.>

-- | Only zips lists of the same length
strictZip :: [a] -> [b] -> Maybe [(a,b)]
strictZip [] []             = Just []
strictZip (e1:ls1) (e2:ls2) = ((e1,e2):) <$> strictZip ls1 ls2
strictZip _ _               = Nothing

-- FIXME: use MonadError instead of partial function
strictZip' :: [a] -> [b] -> [(a,b)]
strictZip' = fromMaybe (error "Internal Error: strictZip'") `ddot` strictZip

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast ls = Just $ last ls

-- | Useful for getting the last value, i.e.
-- mconcat $ map makeLast [1..5] == makeLast 5
makeLast :: a -> Dual (Alt Maybe a)
makeLast = Dual . Alt . Just

getLast :: Dual (Alt f a) -> f a
getLast = getAlt . getDual
