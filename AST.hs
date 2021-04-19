module AST where

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

data ConstStmt =
    CStmtStr String String -- var, string
    deriving Show

data Function = CFunc String [Stmt] [String] deriving Show -- name, code

type AST = ([ConstStmt], [Function], [Stmt])

data Expr = 
    Variabl String | 
    Immediate Int |
    Expr Char Expr Expr |
    FuncExpr String [Expr]
    deriving Show


getConstLabel :: ConstStmt -> String
getConstLabel (CStmtStr name _) = name