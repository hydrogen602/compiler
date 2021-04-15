module AST where

data Stmt = 
        LetStmt String Expr
      | AssignStmt String Expr
      | PrintStmt Bool Expr
      | PrintLiteralStmt Bool String
      | IfStmt Expr [Stmt] [Stmt]
      deriving Show

data ConstStmt =
    CStmtStr String String -- var, string
    deriving Show

type AST = ([ConstStmt], [Stmt])

data Expr = 
    Variabl String | 
    Immediate Int |
    ExprPlus Expr Expr
    deriving Show


getConstLabel :: ConstStmt -> String
getConstLabel (CStmtStr name _) = name