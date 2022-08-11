{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections     #-}

module IRGen.CodeGenIR (generate) where

import           Control.Monad              (forM_, unless, void)
import           Control.Monad.State.Strict (evalState)
import           Data.String.Transform      (toShortByteString)
import qualified Data.Text.Lazy             as T
import           LLVM.AST                   hiding (Function, FunctionType,
                                             Instruction)
import qualified LLVM.AST.Constant          as C
import           LLVM.AST.IntegerPredicate  (IntegerPredicate (NE))
import qualified LLVM.AST.Type              as Types
import qualified LLVM.IRBuilder             as Module
import qualified LLVM.IRBuilder.Instruction as I
import           LLVM.Prelude               (sequenceA_, traverse_)
import           LLVM.Pretty                (ppllvm)

import           Core.Classes               (Nameable (..))
import           Core.CompileResult         (fromSuccess)
import           Core.Types                 (BinaryOp (..), Expr (..),
                                             Function (..), Program (..),
                                             Stmt (..))
import           Extras.FixedAnnotated      (FixedAnnotated (getValue))
import           IRGen.Basics
import           IRGen.Lib
import           IRGen.MixedFunctions       (addition, lessThan)
import           IRGen.Types
import           Types.Addon                (MaybeTyped (..), Typed (..),
                                             isType, typeCheck, typeCheck',
                                             typeCheckFunction)
import qualified Types.Core                 as Ty


generate :: Program MaybeTyped -> T.Text
generate = ppllvm . generateModule


generateModule :: Program MaybeTyped -> Module
generateModule (Program func_mapping _ code' file) = evalState (fromSuccess m) newProgramEnv
  where
    funcs' = mapM_ (withNewScope . generateFuncs) func_mapping

    main :: LLVM Operand
    main = withNewScope $ Module.function "main" [] Types.i32 $ \[] -> do
      forM_ code' generateStmt

    code_state = do
      generateLib
      funcs'
      main

    m = withFile file $ Module.buildModuleT "main" code_state


generateExpr :: MaybeTyped (Expr MaybeTyped) -> CodeGen (Typed Operand)
generateExpr (MaybeTyped maybeExprTy expr) = typeCheck' maybeExprTy $ helper expr
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

      asOperand1 <- generateExpr e1
      asOperand2 <- generateExpr e2
      f asOperand1 asOperand2

    helper (FuncExpr f_name parameters) = do
      func <- lookupFunction f_name
      params_ops <- traverse generateExpr parameters
      (Typed ret f) <- typeCheckFunction func params_ops

      fmap (Typed ret) $ I.call f $ map ((,[]) . getValue) params_ops


generateStmt :: Stmt MaybeTyped -> CodeGen ()
generateStmt = \case
  LetMutStmt pos lv ex          -> withPosition pos $ do
    val <- generateExpr ex
    -- see https://llvm.org/docs/LangRef.html#store-instruction
    void $ makeNewVar Mutable val lv
  LetStmt pos lv ex          -> withPosition pos $ do
    val <- generateExpr ex
    void $ makeNewVar Frozen val lv
  AssignStmt pos lv ex       -> withPosition pos $ do
    var <- lookupVariableMutable lv
    pre_val <- generateExpr ex
    val <- typeCheck (type_ var) pre_val
    I.store (getValue var) 0 (getValue val)
  FuncCall f_name exs        -> do
    func <- lookupFunction f_name

    params_ops <- traverse generateExpr exs
    (Typed ret f) <- typeCheckFunction func params_ops
    fmap (Typed ret) $ I.call f $ map ((,[]) . getValue) params_ops
    pure ()
  IfStmt ex sts_then sts_else     -> mdo
    cond <- generateExpr ex
    b <- if cond `isType` Ty.bool then
        pure cond
      else
        Typed Ty.bool <$> I.icmp NE (getValue cond) (ConstantOperand $ C.Int 32 0)

    Module.condBr (getValue b) then_block else_block

    then_block <- Module.block `Module.named` "then"
    traverse_ generateStmt sts_then
    checkForExit $ Module.br merge_block

    else_block <- Module.block `Module.named` "else"
    traverse_ generateStmt sts_else
    checkForExit $ Module.br merge_block

    merge_block <- Module.block `Module.named` "merge"
    pure ()
  WhileStmt ex sts       -> mdo
    Module.br cond_block
    cond_block <- Module.block `Module.named` "while_condition"
    cond <- generateExpr ex
    b <- if cond `isType` Ty.bool then
        pure cond
      else
        Typed Ty.bool <$> I.icmp NE (getValue cond) (ConstantOperand $ C.Int 32 0)

    Module.condBr (getValue b) while_body break_block

    while_body <- Module.block `Module.named` "while_body"
    traverse_ generateStmt sts
    Module.br cond_block

    break_block <- Module.block `Module.named` "while_break"
    pure ()
  ReturnStmt ex          -> do
    val <- generateExpr ex
    sequenceA_ $ I.ret <$> val


generateFuncs :: Function MaybeTyped -> LLVM (Typed Operand)
generateFuncs (Function pos func_name params ret code _) = withPosition pos $ mdo
  let
    param_names = map (toShortByteString . getName . getValue) params
    param_types = map type_ params

  param_llvm_types <- traverse lookupType param_types
  ret_llvm_type <- lookupType $ type_ ret

  let
    params_with_types = zip param_llvm_types $ map Module.ParameterName param_names

    add_types = Typed (Ty.FunctionType param_types $ type_ ret)

  f <- fmap add_types $ Module.function (toLLVMName func_name) params_with_types ret_llvm_type $ \param_ops -> do
    let params_pairs = zip params param_ops
    traverse_ (\(typed_name, op) -> do
      let typed_op = op <$ typed_name
      makeNewVar Frozen typed_op (getValue typed_name)
      ) params_pairs

    forM_ code generateStmt

  addFunction func_name f
  pure f


checkForExit :: CodeGen () -> CodeGen ()
checkForExit m = do
 check <- Module.hasTerminator
 unless check m
