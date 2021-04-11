module Asm (Line(..), AsmData(..), ASM, 
    generateText, asmPrintInt, asmPrintReg, 
    asmSetToRegister, asmSetToImmediate, asmPrintConstStr, 
    asmAddRegisters, asmAddImmediate
    ) where

import Data.List
import AST

data Line =
      Instruction String [String] -- command, args
    | Label String
    | EmptyLine

data AsmData =
    AsmString String String -- label, data

type ASM = ([Line], [AsmData])

generateDataLine :: AsmData -> String
generateDataLine (AsmString name value) = 
    let len = length name
        padNum = max 1 (8 - 1 - len)
        padding = concat $ replicate padNum " "
    in name ++ ":" ++ padding ++ ".asciiz     \"" ++ value ++ "\""

generateLine :: Line -> String
generateLine (Instruction cmd args) = 
    let len = length cmd
        argLine = intercalate ", " args

        padNum = max 1 (12 - len)
        padding = concat $ replicate padNum " "
    in "        " ++ cmd ++ padding ++ argLine
generateLine (Label name) = name ++ ":"
generateLine EmptyLine = ""

header :: [Line]
header = [
    Instruction ".text" [],
    Instruction ".globl" ["main"],
    Label "main",
    Instruction "addi" ["$sp", "$sp", "-4"],
    Instruction "sw" ["$ra", "0($sp)"],
    EmptyLine
    ]

footer :: [Line]
footer = [
    EmptyLine,
    Instruction "lw" ["$ra", "0($sp)"],
    Instruction "addi" ["$sp", "$sp", "4"],
    Instruction "jr" ["$ra"],
    Instruction ".end" ["main"],
    EmptyLine,
    Instruction ".data" []
    ]

generateText :: ASM -> String
generateText (lines, aData) =
    let allLines = header ++ lines ++ footer
        text = intercalate "\n" (map generateLine allLines)
        textData = intercalate "\n" (map generateDataLine aData)

    in text ++ "\n" ++ textData

asmPrintReg :: String -> [Line]
asmPrintReg register = [
    Instruction "move" ["$a0", register],
    Instruction "li" ["$v0", "1"],
    Instruction "syscall" []
    ] 

asmPrintInt :: Int -> [Line]
asmPrintInt n = [
    Instruction "li" ["$a0", show n],
    Instruction "li" ["$v0", "1"],
    Instruction "syscall" []
    ] 

asmPrintConstStr :: String -> [Line]
asmPrintConstStr label = [
    Instruction "la" ["$a0", label],
    Instruction "li" ["$v0", "4"],
    Instruction "syscall" []
    ] 

asmSetToRegister :: String -> String -> [Line]
asmSetToRegister r1 r2 = [Instruction "move" [r1, r2]]

asmSetToImmediate :: String -> Int -> [Line]
asmSetToImmediate r n = [Instruction "li" [r, show n]]

asmAddRegisters :: String -> String -> String -> [Line]
asmAddRegisters result r1 r2 = [Instruction "add" [result, r1, r2]]

asmAddImmediate :: String -> String -> Int -> [Line]
asmAddImmediate result r1 n = [Instruction "addi" [result, r1, show n]]
