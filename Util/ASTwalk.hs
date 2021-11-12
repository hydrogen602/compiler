module Util.ASTwalk where
import Util.Scope
import AST
import Control.Monad.State  
import Data.Foldable ( Foldable(fold) )
import Data.Functor ( (<&>) )
import Util.AST2
import Util.CompileResult

-- data FunctionToolSet r s o = FunctionToolSet {
--     updateRS :: Stmt -> r -> s -> (r, s),
--     convertStmt :: Stmt -> r -> s -> o,
--     updateRSExpr :: Expr -> r -> s -> (r, s),
--     convertExpr :: Expr -> r -> s -> State s o
-- }


-- treeWalker :: Monoid o => AST -> s -> r -> (o, s)
-- treeWalker ast@(consts, funcs, outer_stmts) initState initRecurs =
--     let global = Global funcs consts []

--         -- update the recursive state and stateful state
--         updateRS :: Stmt -> r -> s -> (r, s)
--         updateRS _ r s = (r, s) -- TODO

--         -- transform a statement into something
--         convertStmt :: Monoid o => Stmt -> r -> s -> o
--         convertStmt st recurs stat = mempty -- TODO

--         updateRSExpr :: Expr -> r -> s -> (r, s)
--         updateRSExpr _ r s = (r, s) -- TODO

--         -- transform a expression into something
--         -- should not recurse, this is dealt with
--         convertExpr :: Monoid o => Expr -> r -> s -> State s o
--         convertExpr expr recurs stat = return mempty -- TODO

--         internalExpr :: Monoid o => Expr -> r -> State s o
--         internalExpr expr oldRecurs = do
--             let internalExpr' = flip internalExpr oldRecurs 
--             outExpr <- case expr of
--               Variabl s -> return mempty
--               Immediate n -> return mempty
--               Expr c ex1 ex2 -> do
--                   out1 <- internalExpr' ex1
--                   out2 <- internalExpr' ex2
--                   return $ out1 `mappend` out2
--               FuncExpr s exs -> mapM internalExpr' exs <&> fold

--             oldState <- get
--             let (midRecurs, midState) = updateRSExpr expr oldRecurs oldState
--             put midState

--             out <- convertExpr expr midRecurs midState

--             return $ outExpr `mappend` out

--         -- walk through the statements
--         internal :: Monoid o => [Stmt] -> r -> State s o
--         internal [] _ = return mempty
--         internal (st:stmts) oldRecurs = do
--             let internalExpr' = flip internalExpr oldRecurs 
--             -- first work on the expressions
--             outExpr <- case st of 
--                 LetStmt s ex -> internalExpr' ex
--                 AssignStmt s ex -> internalExpr' ex
--                 PrintStmt b ex -> internalExpr' ex
--                 PrintLiteralStmt b s -> return mempty
--                 FuncCall s exs -> mapM internalExpr' exs <&> fold
--                 IfStmt ex sts1 sts2 -> internalExpr' ex
--                 WhileStmt ex sts -> internalExpr' ex
--                 ReturnStmt s -> return mempty
            
--             -- update recurs and state given stmt st
--             oldState <- get
--             let (midRecurs, midState) = updateRS st oldRecurs oldState
--             put midState
--             let out = convertStmt st midRecurs midState

--             -- do the recursion: rest of stmts
--             outRest <- internal stmts midRecurs

--             return (outExpr `mappend` out `mappend` outRest)

--     in runState (internal outer_stmts initRecurs) initState -- runState 


-- treeWalker :: SubAST -> s -> Scope -> (SubAST2, s)
-- treeWalker subAst initState initRecurs =
--     let global = initRecurs

--         -- update the recursive state and stateful state
--         updateRS :: Stmt -> Scope -> s -> CompileResult (Scope, s)
--         updateRS (LetStmt name expr) r s = do --(r, s) -- TODO
--             scopeNew <- declare r name
--             return (scopeNew, s)
--         updateRS (IfStmt expr st1 st2) scope s = let
--             (a2,s2) = runState s $ internal st1 (newScope scope)
--             return 



--         -- transform a statement into something
--         convertStmt :: Stmt -> Scope -> s -> SubAST2
--         convertStmt st recurs stat = mempty -- TODO

--         updateRSExpr :: Expr -> Scope -> s -> (Scope, s)
--         updateRSExpr _ r s = (r, s) -- TODO

--         -- transform a expression into something
--         -- should not recurse, this is dealt with
--         convertExpr :: Expr -> Scope -> s -> State s SubAST2
--         convertExpr expr recurs stat = return mempty -- TODO

--         internalExpr ::  Expr -> Scope -> State s SubAST2
--         internalExpr expr oldRecurs = do
--             let internalExpr' = flip internalExpr oldRecurs 
--             outExpr <- case expr of
--               Variabl s -> return mempty
--               Immediate n -> return mempty
--               Expr c ex1 ex2 -> do
--                   out1 <- internalExpr' ex1
--                   out2 <- internalExpr' ex2
--                   return $ out1 `mappend` out2
--               FuncExpr s exs -> mapM internalExpr' exs <&> fold

--             oldState <- get
--             let (midRecurs, midState) = updateRSExpr expr oldRecurs oldState
--             put midState

--             out <- convertExpr expr midRecurs midState

--             return $ outExpr `mappend` out

--         -- walk through the statements
--         internal :: [Stmt] -> Scope -> State s SubAST2
--         internal [] _ = return mempty
--         internal (st:stmts) oldRecurs = do
--             let internalExpr' = flip internalExpr oldRecurs 
--             -- first work on the expressions
--             outExpr <- case st of 
--                 LetStmt s ex -> internalExpr' ex
--                 AssignStmt s ex -> internalExpr' ex
--                 PrintStmt b ex -> internalExpr' ex
--                 PrintLiteralStmt b s -> return mempty
--                 FuncCall s exs -> mapM internalExpr' exs <&> fold
--                 IfStmt ex sts1 sts2 -> internalExpr' ex
--                 WhileStmt ex sts -> internalExpr' ex
--                 ReturnStmt s -> return mempty
            
--             -- update recurs and state given stmt st
--             oldState <- get
--             let (midRecurs, midState) = updateRS st oldRecurs oldState
--             put midState
--             let out = convertStmt st midRecurs midState

--             -- do the recursion: rest of stmts
--             outRest <- internal stmts midRecurs

--             return (outExpr `mappend` out `mappend` outRest)

--     in runState (internal subAst initRecurs) initState -- runState 