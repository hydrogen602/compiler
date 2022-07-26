{-# LANGUAGE FlexibleContexts #-}

module IRGen.Types where

import           Control.Monad.State.Strict (MonadState, State, gets, modify)
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import qualified LLVM.AST                   as L
import qualified LLVM.IRBuilder             as Module



import           Control.Monad.Identity     (Identity (runIdentity))
import qualified Extras.Scope               as Scope
import           Types.Core                 (AType, TypeName)
import qualified Types.Core                 as TC
import           Util.Classes               (Empty (empty), Nameable (..))
import           Util.CompileResult         (fromSuccess)
import           Util.Literals              (ConstValue)
import           Util.Types                 (FunctionName, LocalVariable)


data ProgramEnv = ProgramEnv {
  funcs  :: Map.Map FunctionName L.Operand,
  consts :: Map.Map ConstValue L.Operand,
  locals :: Scope.Scope LocalVariable L.Operand,
  types  :: TC.TypeTracker
}

withNewScope :: MonadState ProgramEnv m => m a -> m a
withNewScope actionInScope = do
  this_scope <- gets locals
  modify (\program -> program{locals=Scope.pushScope $ locals program})
  a <- actionInScope
  modify (\program -> program{locals=this_scope})
  pure a

instance Empty ProgramEnv where
  empty = ProgramEnv mempty mempty empty mempty


lookupType :: MonadState ProgramEnv m => TypeName -> m AType
lookupType name = undefined


addFunction :: MonadState ProgramEnv m => FunctionName -> L.Operand -> m ()
addFunction func_name op =
  modify $ \program -> program{funcs = Map.insert func_name op (funcs program)}


addVariable :: MonadState ProgramEnv m => LocalVariable -> L.Operand -> m ()
addVariable var_name op =
  modify $ \program ->
    let new_locals = runIdentity $ fromSuccess $ Scope.insertUnique var_name op (locals program)
    in program {locals =new_locals}


type LLVM = Module.ModuleBuilderT (State ProgramEnv)
type CodeGen = Module.IRBuilderT LLVM
