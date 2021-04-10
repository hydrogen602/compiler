module AST where

{-
    let n = 0 where n is an element of the Zahlen
    let n = 0 where n \in Zahlen with (-\inf, 5]U(7,9)
-}

data AST = 
    Block [Statement] deriving (Show) 

data Statement =
    -- let n = 0 where n \in Zahlen with (-\inf, 5]
    Declare String String [Constraint] -- var type constraint
    deriving (Show)

data Constraint = 
    Closed String String |
    OpenLeft String String |
    OpenRight String String |
    Open String String
    deriving (Show, Eq)