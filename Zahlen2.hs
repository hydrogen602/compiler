module Zahlen2 (Constraint(EmptySet, MaxOrEq, MinOrEq, Range), Zahl, z, withinBounds, validBounds, isSubSetOf) where

data Constraint =
    EmptySet |
    --Max Integer | 
    MaxOrEq Integer | -- (-inf, x]
    --Min Integer | 
    MinOrEq Integer | -- [x, inf)
    Range Integer Integer deriving Eq;
{-
    1 < x <= 5
    2 <= x <= 5 (x is discrete)
    Range 2 5
-}

instance Show Constraint where
    show EmptySet = "EmptySet"
    show (MaxOrEq x) = "(-inf, " ++ show x ++ "]"
    show (MinOrEq x) = "[" ++ show x ++ ", inf)"
    show (Range a b) = "[" ++ show a ++ ", " ++ show b ++ "]"

data Zahl = Z_ Integer [Constraint] deriving (Show, Eq);

z :: Integer -> [Constraint] -> Zahl
z n c = if validBounds c then
        if withinBounds n c then
            Z_ n c
        else
            error $ "TypeError: " ++ (show n) ++ " is not a member of given set"
    else
        error $ "TypeError: Invalid constraints: " ++ (show c)

laterBounds :: [Constraint] -> Integer -> Bool 
laterBounds [] _ = True
laterBounds [MinOrEq a] lInt = lInt < a
laterBounds ((Range a b):ls) lInt = lInt < a && a <= b && laterBounds ls b
laterBounds _ _ = False

validBounds :: [Constraint] -> Bool 
validBounds [] = True 
validBounds [EmptySet] = True
validBounds [MinOrEq _] = True
validBounds ((MaxOrEq n):ls) = laterBounds ls n
validBounds ((Range a b):ls) = a <= b && laterBounds ls b
validBounds _ = False

withinBounds :: Integer -> [Constraint] -> Bool
withinBounds _ [] = True 
withinBounds n (EmptySet:ls) = False
withinBounds n ((MaxOrEq m):ls) = n <= m || withinBounds n ls
withinBounds n ((MinOrEq m):ls) = n >= m || withinBounds n ls
withinBounds n ((Range m1 m2):ls) = (m1 <= n) && (n <= m2) || withinBounds n ls


isSubSetOf :: [Constraint] -> [Constraint] -> Bool 
isSubSetOf [] [] = True -- both have no constraint
isSubSetOf [] _ = False -- subset unconstraint, but not super 
isSubSetOf _ [] = True  -- subset is constraint, but super is free
isSubSetOf a b = helperIsSubSetOf a b

helperIsSubSetOf :: [Constraint] -> [Constraint] -> Bool
helperIsSubSetOf ((MaxOrEq n):sub) sup@((MaxOrEq m):_) = n <= m && helperIsSubSetOf sub sup
helperIsSubSetOf ((MaxOrEq _):_) _ = False

helperIsSubSetOf ((Range a b): sub) ((MaxOrEq m):super) =
    if b <= m then
        -- range covered by MaxOrEq
        helperIsSubSetOf sub ((MaxOrEq m):super)
    else
        -- range not covered
        helperIsSubSetOf ((Range a b): sub) super

helperIsSubSetOf ((Range a b): sub) ((Range c d):super) =
    if c <= a && b <= d then
        -- range covered
        helperIsSubSetOf sub ((Range c d):super)
    else
        -- not covered
        helperIsSubSetOf ((Range a b):sub) super

helperIsSubSetOf ((Range a b): sub) [MinOrEq m] =
    if a >= m then
        -- range covered
        helperIsSubSetOf sub [MinOrEq m]
    else
        -- not covered
        False

helperIsSubSetOf [MinOrEq n] [MinOrEq m] = n >= m

helperIsSubSetOf [MinOrEq _] _ = False 

helperIsSubSetOf [] _ = True -- all requirements matched

helperIsSubSetOf sub sup = False 
