module AST where

data Stmt = 
        LetStmt String Expr
      | AssignStmt String Expr
      | PrintStmt Expr
      deriving Show

data ConstStmt =
    CStmtStr String String
    deriving Show

type AST = ([ConstStmt], [Stmt])

data Expr = 
    Variabl String | 
    Immediate Int |
    ExprPlus Expr Expr
    deriving Show


getConstLabel :: ConstStmt -> String
getConstLabel (CStmtStr name _) = name