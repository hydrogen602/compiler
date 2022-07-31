{-# LANGUAGE FlexibleContexts #-}

module IRGen.Types where

import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.State.Strict (MonadState (get, put), State, gets,
                                             modify)
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import qualified LLVM.AST                   as L
import qualified LLVM.IRBuilder             as Module

import           Data.Foldable              (traverse_)
import           Extras.PrettyShow          (PrettyShow (pshow))
import           Extras.Scope               (Scope (Scope))
import qualified Extras.Scope               as Scope
import           LLVM.AST                   (Operand)
import           Types.Addon                (Typed (..))
import           Types.Core                 (AType)
import qualified Types.Core                 as TC
import           Util.Classes               (Empty (empty), Nameable (..))
import           Util.CompileResult         (ErrorType (DuplicateNameError, DuplicateTypeError, UnknownFunctionError),
                                             ResultFailed, ResultT, fromSuccess,
                                             throwError)
import           Util.Literals              (ConstValue)
import           Util.Types                 (FunctionName, LocalVariable)


data ProgramEnv = ProgramEnv {
  funcs  :: Map.Map FunctionName (Typed Operand),
  consts :: Map.Map ConstValue (Typed Operand),
  locals :: Scope.Scope LocalVariable (Typed Operand),
  types  :: TC.TypeTracker
}

withNewScope :: MonadState ProgramEnv m => m a -> m a
withNewScope actionInScope = do
  this_scope <- gets locals
  modify (\program -> program{locals=Scope.pushScope $ locals program})
  a <- actionInScope
  modify (\program -> program{locals=this_scope})
  pure a

-- instance Empty ProgramEnv where
--   empty = ProgramEnv mempty mempty empty mempty
newProgramEnv = ProgramEnv mempty mempty empty TC.newTypeTracker


lookupType :: (MonadState ProgramEnv m, MonadError ResultFailed m) => AType -> m L.Type
lookupType name = do
  ty <- gets types
  TC.get name ty


lookupVariable :: (MonadState ProgramEnv m, MonadError ResultFailed m) => LocalVariable -> m (Typed Operand)
lookupVariable name = do
  scope <- gets locals
  scope Scope.!! name


lookupFunction :: (MonadState ProgramEnv m, MonadError ResultFailed m) => FunctionName -> m (Typed Operand)
lookupFunction name = do
  funcMapping <- gets funcs
  case Map.lookup name funcMapping of
    Nothing -> throwError UnknownFunctionError $ pshow name
    Just ty -> pure ty


addFunction :: (MonadState ProgramEnv m, MonadError ResultFailed m) => FunctionName -> Typed Operand -> m ()
addFunction func_name typed@(Typed type_ _) = do
  type_tracker <- gets types
  let all_types_in_func = TC.getContainedTypes type_
  traverse_ (`TC.requireType` type_tracker) all_types_in_func

  program <- get
  if Map.member func_name (funcs program) then
    throwError DuplicateNameError $ pshow func_name
  else
    put $ program{funcs=Map.insert func_name typed (funcs program)}


addVariable :: (MonadState ProgramEnv m, MonadError ResultFailed m) => LocalVariable -> Typed Operand -> m ()
addVariable var_name op = do
  program <- get
  new_locals <- Scope.insertUnique var_name op (locals program)
  put $ program{locals=new_locals}


type LLVM = Module.ModuleBuilderT (ResultT (State ProgramEnv))
type CodeGen = Module.IRBuilderT LLVM
