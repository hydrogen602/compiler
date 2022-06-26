{-# LANGUAGE FlexibleInstances #-}
module Util.Flattened where

import           Control.Monad.State (MonadState (..), State, gets)
import           Data.Foldable       (Foldable (fold))
import           Numeric.Natural     (Natural)

import           Util.Classes        (Nameable (name))
import           Util.Types          (FunctionName, LocalVariable,
                                      UseNewLine (..))
import qualified Util.Types          as Types


-- PseudoVariable are for holding temporary values
-- Useful for splitting up complex expressions
newtype PseudoVariable = PseudoVariable {getPseudoID :: Natural} deriving (Eq, Ord) -- with type in the future

getPseudoVariable :: PseudoVariable -> String
getPseudoVariable i = "$T" ++ show (getPseudoID i)

instance Show PseudoVariable where
  show = getPseudoVariable

instance Nameable PseudoVariable where
  name = getPseudoVariable

type GeneralVariable = Either PseudoVariable LocalVariable

instance Nameable (Either PseudoVariable LocalVariable) where
  name (Left a)  = name a
  name (Right a) = name a

-- Flattend Statements
-- no complex expression but just binary ones
data Stmt2 =
    LetStmt LocalVariable
  | AssignLiteralStmt GeneralVariable Int
  | AssignStmt GeneralVariable GeneralVariable
  | BinaryFunc Char GeneralVariable GeneralVariable
  | PrintStmt UseNewLine GeneralVariable
  | PrintLiteralStmt UseNewLine String
  | FuncCall FunctionName [GeneralVariable] (Maybe GeneralVariable)
  | IfStmt GeneralVariable [Stmt2] [Stmt2]
  | WhileStmt GeneralVariable [Stmt2]
  | ReturnStmt GeneralVariable
  deriving (Show, Eq, Ord)

getNewPseudoVar :: State Natural PseudoVariable
getNewPseudoVar = gets PseudoVariable

getNewGeneralVar :: State Natural GeneralVariable
getNewGeneralVar = Left <$> getNewPseudoVar

transformExpr :: Types.Expr -> GeneralVariable -> State Natural [Stmt2]
transformExpr (Types.Variabl var) target
  | Right var == target = pure []
  | otherwise = pure [AssignStmt target (Right var)]
transformExpr (Types.Immediate n) target = pure [AssignLiteralStmt target n]
transformExpr (Types.Expr op left right) target = do
  var1 <- getNewGeneralVar
  stmts1 <- transformExpr left var1

  var2 <- getNewGeneralVar
  stmts2 <- transformExpr right var2

  pure $ stmts1 ++ stmts2 ++ [BinaryFunc op var1 var2]

transformExpr (Types.FuncExpr func_name expr_ls) target = do
  let
    handleOne expr = do
      var <- getNewGeneralVar
      stmts <- transformExpr expr var
      pure (var, stmts)

  (vars, stmts) <- fmap fold <$> (unzip <$> traverse handleOne expr_ls)

  let call = FuncCall func_name vars (Just target)
  pure $ stmts ++ [call]


transform :: Types.Stmt -> State Natural [Stmt2]
transform (Types.LetStmt var expr) = (LetStmt var :) <$> transformExpr expr (Right var)
transform (Types.AssignStmt var expr) = transformExpr expr (Right var)
transform (Types.PrintStmt useNL expr) = do
  var <- getNewGeneralVar
  stmts <- transformExpr expr var
  pure $ stmts ++ [PrintStmt useNL var]
transform (Types.PrintLiteralStmt useNL s) = pure [PrintLiteralStmt useNL s]
transform (Types.FuncCall func_name expr_ls) = do
  let
    handleOne expr = do
      var <- getNewGeneralVar
      stmts <- transformExpr expr var
      pure (var, stmts)

  (vars, stmts) <- fmap fold . unzip <$> traverse handleOne expr_ls
  pure $ stmts ++ [FuncCall func_name vars Nothing]
transform (Types.IfStmt expr stmts1 stmts2) = do
  varCondition <- getNewGeneralVar
  stmtCondition <- transformExpr expr varCondition

  stmtIf <- transformMany stmts1
  stmtElse <- transformMany stmts2

  pure $ stmtCondition ++ [IfStmt varCondition stmtIf stmtElse]
transform (Types.WhileStmt expr stmts) = do
  varCondition <- getNewGeneralVar
  stmtCondition <- transformExpr expr varCondition

  stmtWhile <- transformMany stmts
  pure $ stmtCondition ++ [WhileStmt varCondition stmtWhile]
transform (Types.ReturnStmt expr) = do
  varResult <- getNewGeneralVar
  stmtResult <- transformExpr expr varResult

  pure $ stmtResult ++ [ReturnStmt varResult]

transformMany :: [Types.Stmt] -> State Natural [Stmt2]
transformMany ls = fold <$> traverse transform ls

