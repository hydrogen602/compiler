module Zahlen where

data Constraint =
    EmptySet |
    --Max Integer | 
    MaxOrEq Integer |
    --Min Integer | 
    MinOrEq Integer |
    Union Constraint Constraint |
    Intersection Constraint Constraint deriving (Show, Eq);
{-
    1 < x <= 5 -> Intersection (MinOrEq 2) (MaxOrEq 5)
    2 <= x <= 5 (x is discrete)
-}

data Zahl = Z_ Integer Constraint;


z :: Integer -> Constraint -> Zahl
z n c = if withinBounds n c then
        Z_ n c
    else
        error $ "TypeError: " ++ (show n) ++ " is not a member of given set"

withinBounds :: Integer -> Constraint -> Bool
withinBounds n EmptySet = False
withinBounds n (MaxOrEq m) = n <= m
withinBounds n (MinOrEq m) = n >= m
withinBounds n (Union c1 c2) = withinBounds n c1 || withinBounds n c2
withinBounds n (Intersection c1 c2) = withinBounds n c1 && withinBounds n c2

simplify :: Constraint -> Constraint
-- union picks the less constraint one
simplify (Union EmptySet b) = b
simplify (Union a EmptySet) = a
simplify (Union (MaxOrEq a) (MaxOrEq b)) = MaxOrEq (max a b)
simplify (Union (MinOrEq a) (MinOrEq b)) = MinOrEq (min a b)

simplify (Union a b) = Union a b

-- intersection picks the more constraint one
simplify (Intersection EmptySet _) = EmptySet
simplify (Intersection _ EmptySet) = EmptySet


simplify (Intersection a b) = undefined
simplify other = other