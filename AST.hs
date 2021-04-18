module AST where

data Stmt = 
        LetStmt String Expr
      | AssignStmt String Expr
      | PrintStmt Bool Expr
      | PrintLiteralStmt Bool String
      | FuncCall String
      | IfStmt Expr [Stmt] [Stmt]
      | WhileStmt Expr [Stmt]
      deriving Show

data ConstStmt =
    CStmtStr String String -- var, string
    deriving Show

data Function = CFunc String [Stmt] deriving Show -- name, code

type AST = ([ConstStmt], [Function], [Stmt])

data Expr = 
    Variabl String | 
    Immediate Int |
    Expr Char Expr Expr
    deriving Show


getConstLabel :: ConstStmt -> String
getConstLabel (CStmtStr name _) = name