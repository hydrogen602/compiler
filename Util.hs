module Util where
import Data
import Data.List

generateTmpReg :: Int -> String 
generateTmpReg n = "$t" ++ if n >= 0 && n < 10 then show n else error "Ran out of temporary registers"

tmpRegs :: [String]
tmpRegs = map (\n -> "$t" ++ show n) [0..9]

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


getUnusedIfEndLabelHelper :: [Line] -> Int 
getUnusedIfEndLabelHelper [] = 0
getUnusedIfEndLabelHelper (Label ('i':'f':'_':'e':'n':'d':'_':num):rest) = 
    let n = (read num :: Int) + 1
        nRest = getUnusedIfEndLabelHelper rest
    in max n nRest
getUnusedIfEndLabelHelper (_:rest) = getUnusedIfEndLabelHelper rest


getUnusedIfEndLabel :: [Line] -> String
getUnusedIfEndLabel = 
    ("if_end_" ++) . show . getUnusedIfEndLabelHelper
