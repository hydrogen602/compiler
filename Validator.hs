module Validator where
import Util.ASTwalk

import AST
import Util.Util
import Util.CompileResult
import Util.Scope
import Debug.Trace
import Control.Monad.Writer  

-- checks if the code is valid. step one: check if vars and funcs exist

findFunctionCallError :: Scope -> Stmt -> CompileResult ()
findFunctionCallError scope (FuncCall name given_params) = --trace "yeeted" $
    let getName (CFunc n _ _) = n
    in do
        (CFunc _ _ req_params) <- nameMaybeToCResult (
            find ((name ==) . getName) (getFunctions scope)) $ "No such function: " ++ name

        if length req_params == length given_params then
            Success ()
        else
            ArgumentError $ "Wrong number of arguments for function: " ++ name    
findFunctionCallError funcs _ = Success ()


findFunctionCallErrorExpr :: Scope -> Expr -> CompileResult ()
findFunctionCallErrorExpr scope (FuncExpr name given_params) = --trace "yeeted" $ -- the sub expression is handled in walkExpr
    let getName (CFunc n _ _) = n -- TODO: all the expr recurse
    in do
        (CFunc _ _ req_params) <- nameMaybeToCResult (
            find ((name ==) . getName) (getFunctions scope)) $ "No such function: " ++ name
        --traceShow ("req_params: " ++ show req_params ++ ", given_params: " ++ show given_params) (
        if length req_params == length given_params then
            Success ()
        else
            ArgumentError $ "Wrong number of arguments for function: " ++ name
findFunctionCallErrorExpr funcs _ = Success ()


walkAST :: [Stmt] -> Scope -> (Scope -> Stmt -> CompileResult ()) -> (Scope -> Expr -> CompileResult ()) -> CompileResult ()
walkAST outer_stmts outer_scope outer_fS outer_fE = 
    let fE = outer_fE outer_scope
        fS = outer_fS outer_scope
        walkAST' a b = walkAST a b outer_fS outer_fE
        
        helper :: [Stmt] -> Scope -> CompileResult ()
        helper [] _ = Success () -- no error
        helper (st@(IfStmt exp stmts1 stmts2):stmts) scope = do --traceShow st $ 
            fE exp
            walkAST' stmts1 (newScope scope)
            walkAST' stmts2 (newScope scope)
            fS st
            walkAST' stmts scope
        helper (st@(WhileStmt exp stmts1):stmts) scope = do --traceShow st $ 
            fE exp
            walkAST' stmts1 (newScope scope)
            fS st
            walkAST' stmts scope
        helper (st@(LetStmt newName expr):stmts) scope = do --traceShow st (case 
            fE expr
            newScope <- declare scope newName
            walkAST' stmts newScope
        helper (st@(AssignStmt name expr):stmts) scope = do --traceShow st (
            fE expr
            referTo scope name
            walkAST' stmts scope
        helper (st@(PrintStmt _ expr):stmts) scope = do --traceShow st (case 
            fE expr
            walkAST' stmts scope
        helper (st@(FuncCall _ exprs):stmts) scope = do --traceShow st $ 
            foldl (\mError e -> do
                        err <- mError
                        walkExpr e fE) (Success ()) exprs
        helper (st:stmts) scope = do --traceShow st $
            fS st
            walkAST' stmts scope

    in helper outer_stmts outer_scope


walkExpr :: Expr -> (Expr -> CompileResult ()) -> CompileResult ()
walkExpr (Expr _ e1 e2) f = do 
    f e1
    f e2
walkExpr e@(FuncExpr _ subExpr) f = do 
    f e
    foldl (\mError e -> do 
            err <- mError
            walkExpr e f) (Success ()) subExpr
walkExpr e f = f e


verify :: AST -> AST
verify tree@(consts, funcs, stmts) =
    let global = Global funcs consts []
         --errorHelper st Nothing = trace "yeet" $ findFunctionCallError funcs st
        --errorHelper _ (Just s) = trace "yeeted" $ Just s

        verifyFunc :: Scope -> Stmt -> CompileResult ()
        verifyFunc scope st = findFunctionCallError scope st -- TODO: use scope

        verifyExpr :: Scope -> Expr -> CompileResult ()
        verifyExpr scope e = findFunctionCallErrorExpr scope e

        --maybeMsg = traceShowId $ applyStmts stmts errorHelper Nothing
    in assertSuccessPassThrough (walkAST stmts global verifyFunc verifyExpr) tree


