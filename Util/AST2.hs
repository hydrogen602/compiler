module Util.AST2 where

import Util.Scope
import AST


data OP = Sym Char | SetVar deriving Show
-- data BasicUnit = 
--     deriving Show

data Stmt2 =
    Quad OP Value Value String | -- OP A1 A2 Result
    Triplet OP Value String | -- OP A Result
    FuncC String [Value] |
    -- Basic BasicUnit |
    If Value SubAST2 SubAST2 |
    While Value SubAST2
    deriving Show

type SubAST2 = [Stmt2]

data FuncDef =
    Main SubAST2 |
    Other String SubAST2
    deriving Show

type AST2 = [FuncDef]

data Value = 
    Var String |
    Lit String |
    ImInt Int
    deriving Show
