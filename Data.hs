module Data where

data Line =
      Instruction String [String] -- command, args
    | Label String
    | EmptyLine
    deriving (Show, Eq)

data AsmData =
    AsmString String String -- label, data

type ASM = ([Line], [AsmData])