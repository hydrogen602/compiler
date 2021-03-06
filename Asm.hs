module Asm where

import Data.List ( intercalate )
import Data
import Util.Util
    ( allSRegisters,
      printNewLineCode,
      printNewLineCall,
      unpackLs1,
      filterBoth,
      mapTup )


generateText :: ASM -> String
generateText (lines, aData) =
    let header :: [Line]
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
            Instruction ".end" ["main"]
            ]
        
        generateLine :: Line -> String
        generateLine (Instruction cmd args) = 
            let len = length cmd
                argLine = intercalate ", " args

                padNum = max 1 (12 - len)
                padding = concat $ replicate padNum " "
            in "        " ++ cmd ++ padding ++ argLine
        generateLine (Label name) = name ++ ":"
        generateLine EmptyLine = ""
        
        generateDataLine :: AsmData -> String
        generateDataLine (AsmString name value) = 
            let len = length name
                padNum = max 1 (8 - 1 - len)
                padding = concat $ replicate padNum " "
            in name ++ ":" ++ padding ++ ".asciiz     \"" ++ value ++ "\""
        generateDataLine (AsmFunc name code) =
            -- addi        $sp, $sp, -4
            -- sw          $ra, 0($sp)
            
            -- lw          $ra, 0($sp)
            -- addi        $sp, $sp, 4
            let registers = "$ra":allSRegisters
                memSpace = 4 * length registers

                regWithMemOffset = zip registers [0,4..]

                sp = Instruction "addi" ["$sp", "$sp", show $ -memSpace]
                sw = map (\(reg, offset) -> Instruction "sw" [reg, show offset ++ "($sp)"]) regWithMemOffset

                lw = map (\(reg, offset) -> Instruction "lw" [reg, show offset ++ "($sp)"]) regWithMemOffset
                spEnd = Instruction "addi" ["$sp", "$sp", show memSpace]
                lines = EmptyLine:Label name:sp:sw ++ EmptyLine:code ++ EmptyLine:lw ++ spEnd:[Instruction "jr" ["$ra"], EmptyLine]
            in  intercalate "\n" (map generateLine lines)


        includePrintNL = elem (unpackLs1 printNewLineCall) lines
        includes = if includePrintNL then printNewLineCode else []

        allLines = header ++ lines ++ footer ++ includes ++ [EmptyLine]
        text = intercalate "\n" (map generateLine allLines)

        helper :: AsmData -> Bool
        helper (AsmFunc _ _) = True
        helper _ = False

        --[Instruction ".data" []]
        (funcs, other) = mapTup generateDataLine $ filterBoth helper aData
        textData = intercalate "\n" $ funcs ++ map generateLine [EmptyLine, Instruction ".data" []] ++ other

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

asmLessThanRegisters :: String -> String -> String -> [Line]
asmLessThanRegisters result r1 r2 = [Instruction "slt" [result, r1, r2]]

asmLessThanImmediate :: String -> String -> Int -> [Line]
asmLessThanImmediate result r1 n = [Instruction "slti" [result, r1, show n]]

asmDoStuffToRegisters :: Char -> (String -> String -> String -> [Line])
asmDoStuffToRegisters '+' = asmAddRegisters
asmDoStuffToRegisters '<' = asmLessThanRegisters
asmDoStuffToRegisters char = error $ "Unknown operator (registers): " ++ [char]

asmDoStuffImmediate :: Char -> (String -> String -> Int -> [Line])
asmDoStuffImmediate '+' = asmAddImmediate
asmDoStuffImmediate '<' = asmLessThanImmediate
asmDoStuffImmediate char = error $ "Unknown operator (immediate): " ++ [char]