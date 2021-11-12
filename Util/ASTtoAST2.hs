module Util.ASTtoAST2 where

import Util.Scope
import AST
import Control.Monad.State  
import Data.Foldable ( Foldable(fold) )
import Data.Functor ( (<&>) )
import Util.AST2
import Util.CompileResult

helper :: Monad m => (m a, m (b, Maybe String)) -> m (a, b, Maybe String)
helper (m1, (m2)) = do
    t1 <- m1
    (t2, ms) <- m2
    return (t1, t2, ms)

transformExpr :: Expr -> Scope -> CompileResult (SubAST2, Maybe String)
transformExpr = undefined

transform :: SubAST -> Scope -> CompileResult SubAST2
transform [] scope = return []
transform (st:stmts) scope = do
    let transformExpr' = flip transformExpr scope
    (scope2, ast2, mTmpVar) <- helper $ case st of
        LetStmt s ex -> (declare scope s, transformExpr' ex)
        AssignStmt s ex -> (return scope, transformExpr' ex)
        PrintStmt b ex -> (return scope, transformExpr' ex)
        PrintLiteralStmt b s -> (return scope, mempty)
        FuncCall s exs -> (return scope, foldMap transformExpr' exs )
        IfStmt ex sts sts' -> (return scope, transformExpr' ex)
        WhileStmt ex sts -> (return scope, transformExpr' ex)
        ReturnStmt s -> (return scope, mempty)
    
    stmtAST2 <- case st of
      LetStmt s ex -> return [Triplet SetVar (Var "tmp") s]
      AssignStmt s ex -> return []
      PrintStmt b ex -> undefined
      PrintLiteralStmt b s -> undefined
      FuncCall s exs -> undefined
      IfStmt ex sts1 sts2 -> do
        sts1' <- transform sts1 (newScope scope2)
        sts2' <- transform sts2 (newScope scope2)
        return undefined
      WhileStmt ex sts -> do
          sts' <- transform sts (newScope scope2)
          return undefined
      ReturnStmt s -> undefined
    
    return $ ast2 `mappend` stmtAST2
