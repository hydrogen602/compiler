module Token where

-- The token type:
data Token =
    NewLine     |
    Let         |
    If          |
    Else        |
    While       |
    Def         |
    Equals      |
    LParens     |
    RParens     |
    LCurly      |
    RCurly      |
    LSqB        |
    RSqB        |
    Comma       |
    Return      |
    Print Bool  |
    Const       |
    Sym Char    |
    Var String  |
    Integer Int |
    Str String
    deriving (Eq,Show)