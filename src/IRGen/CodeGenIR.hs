{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections     #-}

module IRGen.CodeGenIR (generate) where

import           Control.Monad              (forM_, unless)
import           Control.Monad.State.Strict (evalState, gets)
import qualified Data.Map                   as Map
import           Data.String.Transform      (toShortByteString)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.IO          as TIO
import           LLVM.AST                   hiding (Function, FunctionType,
                                             Instruction)
import qualified LLVM.AST                   as L
import           LLVM.AST.Constant          (Constant (GlobalReference))
import qualified LLVM.AST.Constant          as C
import           LLVM.AST.IntegerPredicate  (IntegerPredicate (NE, SLT))
import qualified LLVM.AST.Type              as Types
import qualified LLVM.IRBuilder             as Module
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Monad       as M
import           LLVM.Prelude               (ShortByteString, sequenceA_,
                                             traverse_)
import           LLVM.Pretty                (ppllvm)

import qualified Extras.Scope               as Scope
import           Util.Classes               (Empty (empty), Nameable (..))
import           Util.Literals              (ConstValue)
import           Util.Types                 (Expr (..), Function (..),
                                             FunctionName (FunctionName),
                                             LocalVariable, Op (..),
                                             Program (..), Stmt (..),
                                             UseNewLine (NoUseNewLine, UseNewLine))

import           Control.Monad.Trans.Except (runExceptT)
import           Extras.Conversion          (Into (into))
import           Extras.Misc                (FixedAnnotated (getValue))
import           IRGen.MixedFunctions       (addition, lessThan)
import           IRGen.Types
import           Types.Addon                (MaybeTyped (..), Typed (..),
                                             isType, toMaybeTyped, typeCheck,
                                             typeCheck', typeCheckFunction)
import qualified Types.Core                 as Ty
import           Util.CompileResult         (fromSuccess)


generate :: Program MaybeTyped -> T.Text
generate = ppllvm . generateModule


generateLib :: LLVM ()
generateLib = do
  let
    lib = [
      (Ty.FunctionType [Ty.i32] Ty.i32, "print___i32"),
      (Ty.FunctionType [Ty.i32] Ty.i32, "println___i32")
      ]

  traverse_ (\(ftype@(Ty.FunctionType args out), f_name) -> do
    args_t <- traverse lookupType args
    out_t <- lookupType out
    f <- Module.extern (mkName f_name) args_t out_t
    addFunction (FunctionName f_name) (Typed ftype f)
    ) lib


generateModule :: Program MaybeTyped -> Module
generateModule (Program func_mapping consts code) = evalState (fromSuccess m) empty
  where
    funcs = mapM_ (withNewScope . generateFuncs) func_mapping

    main :: LLVM Operand
    main = withNewScope $ Module.function "main" [] Types.i32 $ \[] -> do
      forM_ code generateStmt

    code_state = do
      generateLib
      funcs
      main

    m = Module.buildModuleT "main" code_state


generateExpr :: MaybeTyped (Expr MaybeTyped) -> CodeGen (Typed Operand)
generateExpr (MaybeTyped type_ expr) = typeCheck' type_ $ helper expr
  where
    helper :: Expr MaybeTyped -> CodeGen (Typed Operand)
    helper (Variabl name)         = do
      op <- lookupVariable name
      sequenceA $ flip I.load 0 <$> op
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

    helper (FuncExpr f_name params) = do
      func <- lookupFunction f_name

      params_ops <- traverse generateExpr params
      (Typed ret f) <- typeCheckFunction func params_ops
      fmap (Typed ret) $ I.call f $ map ((,[]) . getValue) params_ops


makeNewVar :: Typed LocalVariable -> CodeGen (Typed Operand)
makeNewVar (Typed ty lv) = do
  let
    var = LocalReference Types.i32 $ toLLVMName lv
  var <- Typed ty <$> I.alloca Types.i32 Nothing 0
  addVariable lv var
  pure var


generateStmt :: Stmt MaybeTyped -> CodeGen ()
generateStmt = \case
  LetStmt lv ex          -> do
    val <- generateExpr ex
    var <- makeNewVar $ lv <$ val
    -- see https://llvm.org/docs/LangRef.html#store-instruction
    I.store (getValue var) 0 (getValue val)
  AssignStmt lv ex       -> do
    var <- (Scope.! lv) <$> gets locals
    val <- generateExpr ex
    typeCheck (type_ var) val
    I.store (getValue var) 0 (getValue val)
  PrintStmt unl ex       -> do
    var <- generateExpr ex
    typeCheck Ty.i32 var
    func_mapping <- gets funcs
    let
      f_name = case unl of
        UseNewLine   -> FunctionName "println___i32"
        NoUseNewLine -> FunctionName "print___i32"
    -- ToDo: remove this in favor of generic functions
    f <- getValue <$> lookupFunction f_name
    I.call f $ map (,[]) [getValue var]
    pure ()
  PrintLiteralStmt unl s -> error "Not yet implemented"
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
generateFuncs (Function func_name params ret code literals) = mdo
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
    traverse_ (\(name, op) -> do
      -- TODO: copying all args doesn't seem great
      var_op <- makeNewVar name
      I.store (getValue var_op) 0 op
      ) params_pairs

    forM_ code generateStmt

  addFunction func_name f
  pure f


toLLVMName :: Nameable a => a -> Name
toLLVMName = mkName . getName

checkForExit :: CodeGen () -> CodeGen ()
checkForExit m = do
 check <- Module.hasTerminator
 unless check m
