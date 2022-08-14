{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections     #-}

module IRGen.StatementGen where

import           Control.Monad              (unless, void)
import           LLVM.AST                   hiding (Function, FunctionType,
                                             Instruction)
import qualified LLVM.AST.Constant          as C
import qualified LLVM.IRBuilder             as Module
import qualified LLVM.IRBuilder.Instruction as I
import           LLVM.Prelude               (sequenceA_, traverse_)

import           Core.Types                 (BinaryOp (..), Expr (..),
                                             Stmt (..))
import           Extras.FixedAnnotated      (FixedAnnotated (getValue))
import           IRGen.Basics               (getVarValue, makeNewVar, toBool)
import           IRGen.MixedFunctions       (addition, lessThan)
import           IRGen.Types                (CodeGen,
                                             Mutability (Frozen, Mutable),
                                             lookupFunction,
                                             lookupVariableMutable,
                                             withNewScope, withPosition)
import           Types.Addon                (MaybeTyped (..), Typed (..),
                                             typeCheck, typeCheck',
                                             typeCheckFunction)
import qualified Types.Core                 as Ty


generateExpr :: MaybeTyped (Expr MaybeTyped) -> CodeGen (Typed Operand)
generateExpr (MaybeTyped maybeExprTy expr) = do
  expr_operand <- helper expr
  typeCheck' maybeExprTy expr_operand
  where
    helper :: Expr MaybeTyped -> CodeGen (Typed Operand)
    helper (Variabl name)         = getVarValue name
    helper (Immediate n)          =
      pure $ Typed Ty.i32 $ ConstantOperand $ C.Int 32 (fromIntegral n)
    helper (Expr op e1 e2)        = do
      let
        f = case op of
          ADD       -> addition
          LESS_THAN -> lessThan
          _         -> undefined -- FIXME: implement other operators

      asOperand1 <- generateExpr e1
      asOperand2 <- generateExpr e2
      f asOperand1 asOperand2

    helper (FuncExpr f_name parameters) = do
      func <- lookupFunction f_name
      params_ops <- traverse generateExpr parameters
      (Typed ret f) <- typeCheckFunction func params_ops

      fmap (Typed ret) $ I.call f $ map ((,[]) . getValue) params_ops
    helper _ = undefined -- FIXME: implement negation

generateStmt :: Stmt MaybeTyped -> CodeGen ()
generateStmt = \case
  LetMutStmt pos lv ex        -> withPosition pos $ do
    val <- generateExpr ex
    -- see https://llvm.org/docs/LangRef.html#store-instruction
    void $ makeNewVar Mutable val lv
  LetStmt pos lv ex           -> withPosition pos $ do
    val <- generateExpr ex
    void $ makeNewVar Frozen val lv
  AssignStmt pos lv ex        -> withPosition pos $ do
    var <- lookupVariableMutable lv
    pre_val <- generateExpr ex
    val <- typeCheck (type_ var) pre_val
    I.store (getValue var) 0 (getValue val)
  FuncCall f_name exs        -> do
    func <- lookupFunction f_name
    params_ops <- traverse generateExpr exs
    (Typed _ f) <- typeCheckFunction func params_ops
    void $ I.call f $ map ((,[]) . getValue) params_ops
  IfStmt ex sts_then sts_else -> mdo
    cond <- generateExpr ex
    b <- toBool cond

    Module.condBr (getValue b) then_block else_block

    then_block <- Module.block `Module.named` "then"
    withNewScope $ traverse_ generateStmt sts_then
    checkForExit $ Module.br merge_block

    else_block <- Module.block `Module.named` "else"
    withNewScope $ traverse_ generateStmt sts_else
    checkForExit $ Module.br merge_block

    merge_block <- Module.block `Module.named` "merge"
    pure ()
  WhileStmt ex sts       -> mdo
    Module.br cond_block
    cond_block <- Module.block `Module.named` "while_condition"
    cond <- generateExpr ex
    b <- toBool cond

    Module.condBr (getValue b) while_body break_block

    while_body <- Module.block `Module.named` "while_body"
    withNewScope $ traverse_ generateStmt sts
    Module.br cond_block

    break_block <- Module.block `Module.named` "while_break"
    pure ()
  ReturnStmt ex          -> do
    val <- generateExpr ex
    sequenceA_ $ I.ret <$> val


checkForExit :: CodeGen () -> CodeGen ()
checkForExit m = do
 check <- Module.hasTerminator
 unless check m
