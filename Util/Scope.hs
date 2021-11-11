module Util.Scope where

import AST
import Util.CompileResult

type LocalVariable = String -- with type in the future

-- type GlobalScope = ([Function], [ConstStmt], [LocalVariable])
data Scope = Global [Function] [ConstStmt] [LocalVariable] | Local Scope [LocalVariable] deriving Show

getFunctions :: Scope -> [Function]
getFunctions (Global f _ _) = f
getFunctions (Local scope _) = getFunctions scope

getConstStmt :: Scope -> [ConstStmt]
getConstStmt (Global _ c _) = c
getConstStmt (Local scope _) = getConstStmt scope

exists :: Scope -> LocalVariable -> Bool
exists (Global _ _  vars) var = var `elem` vars
exists (Local outerScope vars) var = var `elem` vars || exists outerScope var

referTo :: Scope -> LocalVariable -> CompileResult ()
referTo scope var =
    if exists scope var then
        Success ()
    else 
        NameError $ "Variable " ++ var ++ " is not defined"

declare :: Scope -> LocalVariable -> CompileResult Scope
declare scope@(Global funcs consts vars) var =
    if exists scope var then
        NameError $ "Variable " ++ var ++ " is already declared"
    else 
        Success $ Global funcs consts (var:vars)

declare scope@(Local outerScope vars) var =
    if exists scope var then
        NameError $ "Variable " ++ var ++ " is already declared"
    else 
        Success $ Local outerScope (var:vars)

newScope :: Scope -> Scope
newScope scope = Local scope []