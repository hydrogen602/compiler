module Util where
import Data

generateTmpReg :: Int -> String 
generateTmpReg n = "$t" ++ if n >= 0 && n < 10 then show n else error "Ran out of temporary registers"

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