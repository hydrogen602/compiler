import Control.Monad.State  
import AST

-- next :: [a] -> (a, [a])
-- next [] = error "Empty"
-- next (e:ls) = (e, ls)

-- nextSt :: State [Int] Int
-- nextSt = state next

-- -- next :: State [Int] Int  
-- -- next = do 
-- --     g <- get
-- --     let (e:ls) = g
-- --     put ls
-- --     return e

-- take3 :: State [Int] (Int, Int, Int)
-- take3 = do
--     a <- nextSt
--     b <- nextSt
--     c <- nextSt
--     return (a,b,c)

-- f :: IO ()
-- f =
--     putStrLn "test"

type St = [Function]

funcHelper2 :: [Function] -> State [ConstStmt] [Function]
funcHelper2 [] = state (\st -> ([], st))
funcHelper2 ((CFunc name stmts args):funcs) = do
    postFuncs <- funcHelper2 funcs
    finalStmts <- printLiteralProcessor stmts 
    return (CFunc name finalStmts args:postFuncs)


main = do
    let x = runState (funcHelper2 []) []

    return ()


printLiteralHelper :: Stmt -> State [ConstStmt] Stmt
printLiteralHelper (IfStmt expr ifBlock elseBlock) = do
    ifAst <- printLiteralProcessor ifBlock
    elseAst <- printLiteralProcessor elseBlock
    return (IfStmt expr ifAst elseAst)

printLiteralHelper (WhileStmt expr block) = do
    whileAst <- printLiteralProcessor block
    return (WhileStmt expr whileAst)

printLiteralHelper (PrintLiteralStmt nl s) = state (\consts ->
    let existsAlready :: [ConstStmt] -> Maybe String
        existsAlready [] = Nothing  
        existsAlready (CStmtStr labelName str:ls) =
            if str == s then Just labelName
            else existsAlready ls

        findFreeLabel :: [ConstStmt] -> Int
        findFreeLabel [] = 0
        findFreeLabel (CStmtStr ('s':'t':'r':'_':num) _:ls) =
            let n = 1 + read num :: Int
            in max n (findFreeLabel ls)
        findFreeLabel (_:ls) = findFreeLabel ls

    in case existsAlready consts of 
        (Just label) -> (PrintStmt nl (Variabl label), consts) 
        Nothing -> 
            let label = "str_" ++ show (findFreeLabel consts)
            in  (PrintStmt nl (Variabl label), CStmtStr label s:consts))

printLiteralHelper ls = state (\st -> (ls, st))



printLiteralProcessor :: [Stmt] -> State [ConstStmt] [Stmt]
printLiteralProcessor [] = state (\st -> ([], st))
printLiteralProcessor (st:stmts) = do
    stNew <- printLiteralHelper st
    stmtsFinal <- printLiteralProcessor stmts
    return (stNew:stmtsFinal)
