module Util.Util where
import Data

import Control.Monad.State  
import Data.List

type LabelPrefix = String


type SReg = String
type TReg = String
type AReg = String


generateTmpReg :: Int -> TReg 
generateTmpReg n = "$t" ++ if n >= 0 && n < 10 then show n else error "Ran out of temporary registers"

allTRegisters :: [TReg]
allTRegisters = map (\n -> "$t" ++ show n) [0..9]

allSRegisters :: [SReg]
allSRegisters = map (\n -> "$s" ++ show n) [0..7]

allARegisters :: [AReg]
allARegisters = map (\n -> "$a" ++ show n) [0..3]

printLabel :: String
printLabel = "_printLn"

printNewLineCode :: [Line]
printNewLineCode = [
    EmptyLine,
    Label printLabel,
    Instruction "li" ["$a0", "10"],
    Instruction "li" ["$v0", "11"],
    Instruction "syscall" [],
    Instruction "jr" ["$ra"] 
    ]

printNewLineCall :: [Line]
printNewLineCall = [Instruction "jal" [printLabel]]

unpackLs1 :: [a] -> a
unpackLs1 [x] = x
unpackLs1 _ = error "Expected one element in list"

-- containsSubList :: (Eq a) => [a] -> [a] -> Bool 
-- containsSubList [] _ = True
-- containsSubList _ [] = False 
-- containsSubList pre ls
--     | pre `isPrefixOf` ls = True 
--     | otherwise = containsSubList pre (tail ls)

argumentExtract :: String -> [String] -> Maybe String
argumentExtract _ [] = Nothing 
argumentExtract _ [_] = Nothing 
argumentExtract name (e:arg:args)
    | name == e = Just arg 
    | otherwise = argumentExtract name args


removeLastOf3Tup :: (a, b, c) -> (a, b)
removeLastOf3Tup (a, b, c) = (a, b)

putAndReturn :: MonadState s m => s -> b -> m b
putAndReturn st re = put st >> return re

ddot :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
ddot = (.) . (.)

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
  [] -> Nothing 
  a:_ -> Just a
        