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
    LSqB        |
    RSqB        |
    Comma       |
    Minus       |
    Print       |
    Sym Char    |
    Var String  |
    Integer Integer
    deriving (Eq,Show)