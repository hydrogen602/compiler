module Token where

-- The token type:
data Token =
    NewLine     |
    Let         |
    If          |
    Else        |
    While       |
    Equals      |
    LParens     |
    RParens     |
    LCurly      |
    RCurly      |
    LSqB        |
    RSqB        |
    Comma       |
    Print Bool  |
    Const       |
    Sym Char    |
    Var String  |
    Integer Int |
    Str String
    deriving (Eq,Show)