module Util.ASTwalk where
import Util.Scope
import AST
import Control.Monad.State  
import Data.Foldable ( Foldable(fold) )
import Data.Functor ( (<&>) )


treeWalker :: Monoid o => AST -> s -> r -> (o, s)
treeWalker ast@(consts, funcs, outer_stmts) initState initRecurs =
    let global = Global funcs consts []

        -- update the recursive state and stateful state
        updateRS :: Stmt -> r -> s -> (r, s)
        updateRS = undefined

        -- transform a statement into something
        convertStmt :: Monoid o => Stmt -> r -> s -> o
        convertStmt st recurs stat = mempty

        internalExpr :: Monoid o => Expr -> r -> State s o
        internalExpr = undefined


        -- walk through the statements
        internal :: Monoid o => [Stmt] -> r -> State s o
        internal [] _ = return mempty
        internal (st:stmts) oldRecurs = do
            let internalExpr' = flip internalExpr oldRecurs 
            -- first work on the expressions
            outExpr <- case st of 
                LetStmt s ex -> internalExpr' ex
                AssignStmt s ex -> internalExpr' ex
                PrintStmt b ex -> internalExpr' ex
                PrintLiteralStmt b s -> return mempty
                FuncCall s exs -> do
                    mapM internalExpr' exs <&> fold
                
                IfStmt ex sts1 sts2 -> internalExpr' ex
                WhileStmt ex sts -> internalExpr' ex
                ReturnStmt s -> return mempty
            
            -- update recurs and state given stmt st
            oldState <- get
            let (midRecurs, midState) = updateRS st oldRecurs oldState
            put midState
            let out = convertStmt st midRecurs midState

            -- do the recursion: rest of stmts
            outRest <- internal stmts midRecurs

            return (outExpr `mappend` out `mappend` outRest)



    in runState (internal outer_stmts initRecurs) initState -- runState 