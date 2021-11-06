module AST where

import Data.Foldable (Foldable)
data Stmt = 
        LetStmt String Expr
      | AssignStmt String Expr
      | PrintStmt Bool Expr
      | PrintLiteralStmt Bool String
      | FuncCall String [Expr]
      | IfStmt Expr [Stmt] [Stmt]
      | WhileStmt Expr [Stmt]
      | ReturnStmt String
      deriving Show

applyStmts :: [Stmt] -> (Stmt -> a -> a) -> a -> a
applyStmts ls f input = foldr f input ls
-- applyStmts [] f input = input
-- applyStmts (stmt:ls) f input = f stmt (applyStmts ls f input)

data ConstStmt =
    CStmtStr String String -- var, string
    deriving Show

--data FuncParam = TypedParam String String deriving Show -- var type
type FuncParam = String

data Function = CFunc String [Stmt] [FuncParam] deriving Show -- name, code


type AST = ([ConstStmt], [Function], [Stmt])

data Expr = 
    Variabl String | 
    Immediate Int |
    Expr Char Expr Expr |
    FuncExpr String [Expr]
    deriving Show

getConstLabel :: ConstStmt -> String
getConstLabel (CStmtStr name _) = name