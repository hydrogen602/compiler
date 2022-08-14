module Core.Util where

import           Control.Monad.State

-- FIXME: remove partial function
unpackLs1 :: [a] -> a
unpackLs1 [x] = x
unpackLs1 _   = error "Expected one element in list"

argumentExtract :: String -> [String] -> Maybe String
argumentExtract _ [] = Nothing
argumentExtract _ [_] = Nothing
argumentExtract name (e:arg:args)
    | name == e = Just arg
    | otherwise = argumentExtract name args

removeLastOf3Tup :: (a, b, c) -> (a, b)
removeLastOf3Tup (a, b, _) = (a, b)

putAndReturn :: MonadState s m => s -> b -> m b
putAndReturn st re = put st >> return re

ddot :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
ddot = (.) (.) (.)

dddot :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c
dddot = (.) . (.) . (.)

filterBoth :: (a -> Bool) -> [a] -> ([a], [a])
filterBoth func ls =
    (filter func ls, filter (not . func) ls)

mapTup :: (a -> b) -> ([a], [a]) -> ([b], [b])
mapTup func (ls1, ls2) =
    (map func ls1, map func ls2)

stateHelper :: b -> (a -> (b, a))
stateHelper b st = (b, st)

find :: (a -> Bool) -> [a] -> Maybe a
find f ls = case filter f ls of
  []  -> Nothing
  a:_ -> Just a


(<.>) :: Functor f => (b -> c) -> (a -> f b) -> (a -> f c)
(<.>) f fm = fmap f . fm

infixr 9 <.>

quote :: String -> String
quote s = '"':s ++ "\""
