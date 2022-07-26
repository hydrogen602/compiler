{-# LANGUAGE FlexibleInstances #-}

module Util.Flattened where

import           Control.Monad.State (MonadState (..), State, gets, modify)
import           Data.Foldable       (Foldable (fold))
import qualified Data.Map.Strict     as Map
import           Numeric.Natural     (Natural)

import           Util.Classes        (Empty (empty), Nameable (..))
import           Util.Literals       as Literals2
import           Util.Types          (FunctionName, LocalVariable, Op,
                                      UseNewLine (..))
import qualified Util.Types          as Types


-- PseudoVariable are for holding temporary values
-- Useful for splitting up complex expressions
newtype PseudoVariable = PseudoVariable { getPseudoID :: Natural } deriving (Eq, Ord) -- with type in the future

getPseudoVariable :: PseudoVariable -> String
getPseudoVariable i = "__tmp" ++ show (getPseudoID i)

instance Show PseudoVariable where
  show = getPseudoVariable

instance Nameable PseudoVariable where
  getName = getPseudoVariable

type GeneralVariable = Either PseudoVariable LocalVariable

instance Nameable (Either PseudoVariable LocalVariable) where
  getName (Left a)  = getName a
  getName (Right a) = getName a

-- Flattend Statements
-- no complex expression but just binary ones
data Stmt2 =
    LetStmt LocalVariable
  | AssignLiteralStmt GeneralVariable Int
  | AssignStmt GeneralVariable GeneralVariable
  | BinaryFuncStmt Op GeneralVariable GeneralVariable GeneralVariable
  | PrintStmt UseNewLine GeneralVariable
  | PrintLiteralStmt UseNewLine String
  | FuncCall FunctionName [GeneralVariable] (Maybe GeneralVariable)
  | IfStmt GeneralVariable [Stmt2] [Stmt2]
  | WhileStmt GeneralVariable [Stmt2]
  | ReturnStmt GeneralVariable
  deriving (Show, Eq, Ord)

getNewPseudoVar :: State Natural PseudoVariable
getNewPseudoVar = do
  var <- gets PseudoVariable
  modify (+1)
  pure var

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

  pure $ stmts1 ++ stmts2 ++ [BinaryFuncStmt op target var1 var2]

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


data Function2 = Function2 {
  functionName :: FunctionName,
  params       :: [LocalVariable],
  functionCode :: [Stmt2],
  literals     :: Literals2
} deriving (Show, Eq, Ord)


transformFunction :: Types.Function -> State Natural Function2
transformFunction (Types.Function func_name params function_code literals) = do
  stmts <- transformMany function_code
  pure $ Function2 func_name params stmts literals


data Program2 = Program2 {
  functions :: Map.Map FunctionName Function2,
  constants :: Consts,
  code      :: [Stmt2]
} deriving (Show, Eq, Ord)

newProgram :: Program2
newProgram = Program2 mempty empty mempty


transformProgram :: Types.Program -> State Natural Program2
transformProgram (Types.Program funcs consts code) = do
  funcs2 <- traverse transformFunction funcs
  stmts <- transformMany code

  pure $ Program2 funcs2 consts stmts
