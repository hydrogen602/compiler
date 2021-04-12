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
    Plus        |
    Print Bool  |
    Const       |
    Sym Char    |
    Var String  |
    Integer Int |
    Str String
    deriving (Eq,Show)