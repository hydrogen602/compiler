{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module IRGen.FunctionGen (generateAllFunctions) where

import           Data.String.Transform     (ToShortByteString (toShortByteString))
import           LLVM.AST                  (Operand)
import qualified LLVM.AST                  as L
import qualified LLVM.IRBuilder            as Module

import           Control.Monad             (void)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.State.Class (MonadState)
import           Core.Classes              (Nameable (..))
import           Core.CompileResult        (ResultFailed)
import           Core.Types                (Function (..), FunctionName,
                                            LocalVariable, Stmt (..))
import           Data.Bifunctor            (Bifunctor (second))
import           Data.Foldable             (sequenceA_, traverse_)
import           Extras.FixedAnnotated     (FixedAnnotated (getValue, toPair))
import           Extras.Misc               (strictZip')
import           IRGen.Basics              (makeNewVar, toLLVMName)
import           IRGen.StatementGen        (generateStmt, withNewScopeAndDrop)
import           IRGen.Types               (CodeGen, LLVM, Mutability (Frozen),
                                            ProgramEnv, addFunction, lookupType,
                                            withPosition)
import           Types.Addon               (MaybeTyped (..), Typed (..))
import qualified Types.Core                as Ty


generateAllFunctions :: [Function MaybeTyped] -> LLVM ()
generateAllFunctions funcs = do
  (declarations, definitions) <- unzip <$> traverse setupFunction funcs
  traverse_ (uncurry addFunction) declarations
  sequenceA_ definitions

setupFunction :: Function MaybeTyped -> LLVM ((FunctionName, Typed Operand), LLVM ())
setupFunction (Function pos func_name parameters ret code_ _) = withPosition pos $ do
  (params_with_types, ret_llvm_type) <- inputOutputTyping parameters ret
  let
    func_builder :: Module.MonadModuleBuilder m => ([Operand] -> Module.IRBuilderT m ()) -> m Operand
    func_builder = Module.function (toLLVMName func_name) params_with_types ret_llvm_type

    func_action :: LLVM Operand
    func_action = func_builder $ generateFunctionBody code_ parameters

    add_types :: a -> Typed a
    add_types = Typed $ Ty.FunctionType (map type_ parameters) $ type_ ret

    declaration = add_types $ fst $ Module.runModuleBuilder Module.emptyModuleBuilder dummy_ir_builder

    dummy_ir_builder = func_builder $ const $ pure ()

  pure ((func_name, declaration), void func_action)


inputOutputTyping :: (MonadState ProgramEnv m, MonadError ResultFailed m) => [Typed LocalVariable] -> Typed () -> m ([(L.Type, Module.ParameterName)], L.Type)
inputOutputTyping parameters ret = do
  let
    (param_types, param_names) = unzip $ map (second (toShortByteString . getName) . toPair) parameters

  param_llvm_types <- traverse lookupType param_types
  ret_llvm_type <- lookupType $ type_ ret
  let
    params_with_types = strictZip' param_llvm_types $ map Module.ParameterName param_names
  pure (params_with_types, ret_llvm_type)

generateFunctionBody :: [Stmt MaybeTyped] -> [Typed LocalVariable] -> [Operand] -> CodeGen ()
generateFunctionBody code_ parameters param_ops = withNewScopeAndDrop $ do
  _entry <- Module.block `Module.named` "entry"
  let params_pairs = zip parameters param_ops
  traverse_ (\(typed_name, op) -> do
    let typed_op = op <$ typed_name
    makeNewVar Frozen typed_op (getValue typed_name)
    ) params_pairs

  traverse_ generateStmt code_
