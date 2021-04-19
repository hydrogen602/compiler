module Util where
import Data
import Data.List

type SReg = String
type TmpReg = String
type AReg = String


generateTmpReg :: Int -> TmpReg 
generateTmpReg n = "$t" ++ if n >= 0 && n < 10 then show n else error "Ran out of temporary registers"

tmpRegs :: [TmpReg]
tmpRegs = map (\n -> "$t" ++ show n) [0..9]

allSRegisters :: [SReg]
allSRegisters = ["$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"]

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

