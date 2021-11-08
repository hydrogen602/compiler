module Validator where

import AST
import Util.Util
import Util.CompileResult
import Debug.Trace
import Control.Monad.Writer  

type Error = String
-- checks if the code is valid. step one: check if vars and funcs exist

findFunctionCallError :: [Function] -> Stmt -> CompileResult ()
findFunctionCallError funcs (FuncCall name given_params) = --trace "yeeted" $
    let getName (CFunc n _ _) = n
    in do
        (CFunc _ _ req_params) <- nameMaybeToCResult (find ((name ==) . getName) funcs) name

        if length req_params == length given_params then
            Success ()
        else
            ArgumentError $ "Wrong number of arguments for function: " ++ name      
findFunctionCallError funcs _ = Success ()


findFunctionCallErrorExpr :: [Function] -> Expr -> CompileResult ()
findFunctionCallErrorExpr funcs (FuncExpr name given_params) = --trace "yeeted" $ -- the sub expression is handled in walkExpr
    let getName (CFunc n _ _) = n
    in do
        (CFunc _ _ req_params) <- nameMaybeToCResult (find ((name ==) . getName) funcs) name
        --traceShow ("req_params: " ++ show req_params ++ ", given_params: " ++ show given_params) (
        if length req_params == length given_params then
            Success ()
        else
            ArgumentError $ "Wrong number of arguments for function: " ++ name--)
findFunctionCallErrorExpr funcs _ = Success ()


walkAST :: [Stmt] -> (Stmt -> CompileResult ()) -> (Expr -> CompileResult ()) -> CompileResult ()
walkAST [] fS fE = Success () -- no error
walkAST (st@(IfStmt exp stmts1 stmts2):stmts) fS fE = do --traceShow st $ 
    fE exp
    walkAST stmts1 fS fE
    walkAST stmts2 fS fE
    fS st
    walkAST stmts fS fE

walkAST (st@(WhileStmt exp stmts1):stmts) fS fE = do --traceShow st $ 
    fE exp
    walkAST stmts1 fS fE
    fS st
    walkAST stmts fS fE
walkAST (st@(LetStmt _ expr):stmts) fS fE = do --traceShow st (case 
    fE expr
    walkAST stmts fS fE
walkAST (st@(AssignStmt _ expr):stmts) fS fE = do --traceShow st (
    fE expr
    walkAST stmts fS fE
walkAST (st@(PrintStmt _ expr):stmts) fS fE = do --traceShow st (case 
    fE expr
    walkAST stmts fS fE
walkAST (st@(FuncCall _ exprs):stmts) fS fE = do --traceShow st $ 
    foldl (\mError e -> do
                err <- mError
                walkExpr e fE) (Success ()) exprs

walkAST (st:stmts) fS fE = do --traceShow st $
    fS st
    walkAST stmts fS fE

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
    let --errorHelper st Nothing = trace "yeet" $ findFunctionCallError funcs st
        --errorHelper _ (Just s) = trace "yeeted" $ Just s

        verifyFunc :: Stmt -> CompileResult ()
        verifyFunc st = findFunctionCallError funcs st

        verifyExpr :: Expr -> CompileResult ()
        verifyExpr e = findFunctionCallErrorExpr funcs e

        --maybeMsg = traceShowId $ applyStmts stmts errorHelper Nothing
    in assertSuccessPassThrough (walkAST stmts verifyFunc verifyExpr) tree

