module Validator where

import AST
import Util
import Debug.Trace
import Control.Monad.Writer  

type Error = String
-- checks if the code is valid. step one: check if vars and funcs exist

findFunctionCallError :: [Function] -> Stmt -> Maybe Error
findFunctionCallError funcs (FuncCall name given_params) = trace "yeeted" $
    let getName (CFunc n _ _) = n
        fM = find ((name ==) . getName) funcs
    in case fM of
            Nothing -> Just $ "No such function: " ++ name
            Just (CFunc _ _ req_params) -> 
                traceShow ("req_params: " ++ show req_params ++ ", given_params: " ++ show given_params) (
                if length req_params == length given_params then
                    Nothing
                else
                    Just $ "Wrong number of arguments for function: " ++ name)
findFunctionCallError funcs _ = Nothing


findFunctionCallErrorExpr :: [Function] -> Expr -> Maybe Error
findFunctionCallErrorExpr funcs (FuncExpr name given_params) = trace "yeeted" $ -- the sub expression is handled in walkExpr
    let getName (CFunc n _ _) = n
        fM = find ((name ==) . getName) funcs
    in case fM of
            Nothing -> Just $ "No such function: " ++ name
            Just (CFunc _ _ req_params) -> 
                traceShow ("req_params: " ++ show req_params ++ ", given_params: " ++ show given_params) (
                if length req_params == length given_params then
                    Nothing
                else
                    Just $ "Wrong number of arguments for function: " ++ name)
findFunctionCallErrorExpr funcs _ = Nothing


walkAST :: [Stmt] -> (Stmt -> Maybe Error) -> (Expr -> Maybe Error) -> Maybe Error
walkAST [] fS fE = Nothing -- no error
walkAST (st@(IfStmt exp stmts1 stmts2):stmts) fS fE = traceShow st $ case fE exp of
    Just err -> Just err
    Nothing -> case walkAST stmts1 fS fE of
        Just err -> Just err
        Nothing -> case walkAST stmts2 fS fE of
            Just err -> Just err
            Nothing -> case fS st of
                Nothing -> walkAST stmts fS fE
                Just err -> Just err

walkAST (st@(WhileStmt exp stmts1):stmts) fS fE = traceShow st $ case fE exp of
    Just err -> Just err
    Nothing -> case walkAST stmts1 fS fE of
        Just err -> Just err
        Nothing -> case fS st of
            Nothing -> walkAST stmts fS fE
            Just err -> Just err
walkAST (st@(LetStmt _ expr):stmts) fS fE = traceShow st (case fE expr of
   Nothing -> walkAST stmts fS fE
   Just s -> Just s)
walkAST (st@(AssignStmt _ expr):stmts) fS fE = traceShow st (case fE expr of
   Nothing -> walkAST stmts fS fE
   Just s -> Just s)
walkAST (st@(PrintStmt _ expr):stmts) fS fE = traceShow st (case fE expr of
   Nothing -> walkAST stmts fS fE
   Just s -> Just s)
walkAST (st@(FuncCall _ exprs):stmts) fS fE = traceShow st $ foldl (\mError e -> case mError of
        Nothing -> walkExpr e fE
        Just err -> Just err) Nothing exprs
walkAST (st:stmts) fS fE = traceShow st $
    case fS st of
        Nothing -> walkAST stmts fS fE
        Just err -> Just err


walkExpr :: Expr -> (Expr -> Maybe Error) -> Maybe Error
walkExpr (Expr _ e1 e2) f = case f e1 of
  Just s -> Just s
  Nothing -> f e2
walkExpr e@(FuncExpr _ subExpr) f = case f e of
    Just s -> Just s
    Nothing -> foldl (\mError e -> case mError of
        Nothing -> walkExpr e f
        Just err -> Just err) Nothing subExpr
walkExpr e f = f e
        


verify :: AST -> AST
verify tree@(consts, funcs, stmts) =
    let --errorHelper st Nothing = trace "yeet" $ findFunctionCallError funcs st
        --errorHelper _ (Just s) = trace "yeeted" $ Just s

        verifyFunc :: Stmt -> Maybe Error
        verifyFunc st = findFunctionCallError funcs st

        verifyExpr :: Expr -> Maybe Error
        verifyExpr e = findFunctionCallErrorExpr funcs e

        --maybeMsg = traceShowId $ applyStmts stmts errorHelper Nothing
    in trace "it run" $ case walkAST stmts verifyFunc verifyExpr of
      Nothing -> tree
      Just s -> error $ "Compiler Error: " ++ s

