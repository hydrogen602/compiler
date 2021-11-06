module Validator where

import AST
import Util

-- checks if the code is valid. step one: check if vars and funcs exist

findFunctionCallError :: [Function] -> Stmt -> Maybe String
findFunctionCallError funcs (FuncCall name given_params) = 
    let getName (CFunc n _ _) = n
        fM = find ((name ==) . getName) funcs
    in case fM of
            Nothing -> Just $ "No such function: " ++ name
            Just (CFunc _ _ req_params) -> 
                if length req_params == length given_params then
                    Nothing
                else
                    Just $ "Wrong number of arguments for function: " ++ name
findFunctionCallError funcs _ = Nothing


verify :: AST -> AST
verify tree@(consts, funcs, stmts) =
    let errorHelper st Nothing = findFunctionCallError funcs st
        errorHelper _ (Just s) = Just s

        maybeMsg = applyStmts stmts errorHelper Nothing
    in case maybeMsg of
      Nothing -> tree
      Just s -> error $ "Compiler Error: " ++ s

