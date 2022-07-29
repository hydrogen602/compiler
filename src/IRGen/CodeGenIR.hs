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
import           LLVM.Prelude               (ShortByteString, traverse_)
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
import           IRGen.Types
import           Types.Addon                (TypedOperand (TypedOperand))
import           Types.Core
import           Util.CompileResult         (fromSuccess)


generate :: Program -> T.Text
generate = ppllvm . generateModule


generateLib :: LLVM ()
generateLib = do
  let
    lib = [
      (FunctionType ["i32"] "i32", "print___i32"),
      (FunctionType ["i32"] "i32", "println___i32")
      ]

  traverse_ (\(ftype@(FunctionType args out), f_name) -> do
    args_t <- traverse lookupType args
    out_t <- lookupType out
    f <- Module.extern (mkName f_name) args_t out_t
    addFunction (FunctionName f_name) (TypedOperand ftype f)
    ) lib


generateModule :: Program -> Module
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


generateExpr :: Expr -> CodeGen TypedOperand
generateExpr (Variabl name)         = gets locals >>= flip I.load 0 . (Scope.! name)
generateExpr (Immediate n)          = pure $ ConstantOperand $ C.Int 32 (fromIntegral n)
generateExpr (Expr op e1 e2)        = do
  let
    f = case op of
      ADD       -> I.add
      LESS_THAN -> I.icmp SLT
  asOperand1 <- generateExpr e1
  asOperand2 <- generateExpr e2
  f asOperand1 asOperand2

generateExpr (FuncExpr f_name params) = do
  func_mapping <- gets funcs
  let f = func_mapping Map.! f_name
  params_ops <- traverse generateExpr params
  I.call f $ map (,[]) params_ops


makeNewVar :: LocalVariable -> CodeGen TypedOperand
makeNewVar lv = do
  let
    var = LocalReference Types.i32 $ toLLVMName lv
  var <- I.alloca Types.i32 Nothing 0
  addVariable lv var
  pure var


generateStmt :: Stmt -> CodeGen ()
generateStmt = \case
  LetStmt lv ex          -> do
    var <- makeNewVar lv
    val <- generateExpr ex
    -- see https://llvm.org/docs/LangRef.html#store-instruction
    I.store var 0 val
  AssignStmt lv ex       -> do
    var <- (Scope.! lv) <$> gets locals
    val <- generateExpr ex
    I.store var 0 val
  PrintStmt unl ex       -> do
    var <- generateExpr ex
    func_mapping <- gets funcs
    let
      f_name = case unl of
        UseNewLine   -> "println___i32"
        NoUseNewLine -> "print___i32"
      f = func_mapping Map.! FunctionName f_name
    I.call f $ map (,[]) [var]
    pure ()
  PrintLiteralStmt unl s -> error "Not yet implemented"
  FuncCall fn exs        -> do
    vars <- traverse generateExpr exs
    func_mapping <- gets funcs
    let f = func_mapping Map.! fn

    I.call f $ map (,[]) vars
    pure ()
  IfStmt ex sts_then sts_else     -> mdo
    cond <- generateExpr ex
    b <- I.icmp NE cond (ConstantOperand $ C.Int 32 0)
    Module.condBr b then_block else_block

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
    b <- I.icmp NE cond (ConstantOperand $ C.Int 32 0)
    Module.condBr b while_body break_block

    while_body <- Module.block `Module.named` "while_body"
    traverse_ generateStmt sts
    Module.br cond_block

    break_block <- Module.block `Module.named` "while_break"
    pure ()
  ReturnStmt ex          -> generateExpr ex >>= I.ret


generateFuncs :: Function -> LLVM TypedOperand
generateFuncs (Function func_name params code literals) = mdo
  let param_names = map (toShortByteString . getName) params

  f <- Module.function (toLLVMName func_name) (map ((Types.i32,) . Module.ParameterName) param_names) Types.i32 $ \param_ops -> do
    let params_pairs = zip params param_ops
    traverse_ (\(name, op) -> do
      -- TODO: copying all args doesn't seem great
      var_op <- makeNewVar name
      I.store var_op 0 op
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
