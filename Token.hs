module Token where

-- The token type:
data Token =
    NewLine     |
    Let         |
    In          |
    Equals      |
    Infinity    |
    Union       |
    Where       |
    LParens     |
    RParens     |
    LCurly      |
    RCurly      |
    LSqB        |
    RSqB        |
    Comma       |
    Minus       |
    Print       |
    Sym Char    |
    Var String  |
    Integer Int
    deriving (Eq,Show)