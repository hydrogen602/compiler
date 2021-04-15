module AST where

data Stmt = 
        LetStmt String Expr
      | AssignStmt String Expr
      | PrintStmt Bool Expr
      | PrintLiteralStmt Bool String
      | IfStmt Expr [Stmt] [Stmt]
      | WhileStmt Expr [Stmt]
      deriving Show

data ConstStmt =
    CStmtStr String String | -- var, string
    CFunc String [Stmt] -- name, code
    deriving Show

type AST = ([ConstStmt], [Stmt])

data Expr = 
    Variabl String | 
    Immediate Int |
    Expr Char Expr Expr
    deriving Show


getConstLabel :: ConstStmt -> String
getConstLabel (CStmtStr name _) = name