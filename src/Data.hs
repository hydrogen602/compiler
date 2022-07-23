module Data where

data Line =
      Instruction String [String] -- command, args
    | Label String
    | EmptyLine
    deriving (Show, Eq)

data AsmData =
    AsmString String String | -- label, data
    AsmFunc String [Line]
    deriving (Show)

type ASM = ([Line], [AsmData])
