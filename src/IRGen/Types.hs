{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module IRGen.Types where

import           Control.Monad.Error.Class        (MonadError)
import           Control.Monad.State.Strict       (MonadState (get, put), State,
                                                   gets, modify)
import           Data.Foldable                    (traverse_)
import qualified Data.Map.Strict                  as Map
import qualified LLVM.AST                         as L
import qualified LLVM.IRBuilder                   as Module

import           Compat.Control.Monad.Error.Class (withError)
import           Core.Classes                     (Empty (empty))
import           Core.CompileResult               (ErrorType (DuplicateNameError, ImmutableVariableError, UnknownFunctionError),
                                                   ResultFailed (errFile, errLoc),
                                                   ResultT, throwError)
import           Core.Literals                    (ConstValue)
import           Core.Types                       (FunctionName, LocalVariable)
import           Extras.Position                  (Pos)
import           Extras.PrettyShow                (PrettyShow (pshow))
import qualified Extras.Scope                     as Scope
import           LLVM.AST                         (Operand)
import           Types.Addon                      (Typed (..))
import           Types.Core                       (AType)
import qualified Types.Core                       as TC


data Mutability = Mutable | Frozen deriving (Show, Eq, Ord)
data Variable = Variable {
  mutability :: Mutability,
  variable   :: Typed Operand
  } deriving (Show, Eq, Ord)


data ProgramEnv = ProgramEnv {
  funcs  :: Map.Map FunctionName (Typed Operand),
  consts :: Map.Map ConstValue (Typed Operand),
  locals :: Scope.Scope LocalVariable Variable,
  types  :: TC.TypeTracker
}

withNewScope :: MonadState ProgramEnv m => m a -> m (Scope.DroppedScopes LocalVariable Variable, a)
withNewScope actionInScope = do
  this_scope <- gets locals
  modify (\program -> program{locals=Scope.pushScope $ locals program})
  a <- actionInScope
  final_scope <- gets locals
  modify (\program -> program{locals=this_scope})
  let
    dropped = Scope.collectDroppedScopes final_scope this_scope
  pure (dropped, a)

-- instance Empty ProgramEnv where
--   empty = ProgramEnv mempty mempty empty mempty
newProgramEnv :: ProgramEnv
newProgramEnv = ProgramEnv mempty mempty empty TC.newTypeTracker


lookupType :: (MonadState ProgramEnv m, MonadError ResultFailed m) => AType -> m L.Type
lookupType name = fst <$> lookupType' name

lookupType' :: (MonadState ProgramEnv m, MonadError ResultFailed m) => AType -> m (L.Type, TC.Allocation)
lookupType' name = do
  ty <- gets types
  TC.get name ty


-- lookupType' :: (MonadState ProgramEnv m, MonadError ResultFailed m) => TypeLabel -> m L.Type
-- lookupType' name = do
--   ty <- gets types
--   TC.get' name ty


-- | Lookup a variable for reading, not writing
-- returns both mutable and immutable
lookupVariable :: (MonadState ProgramEnv m, MonadError ResultFailed m) => LocalVariable -> m Variable
lookupVariable name = do
  scope <- gets locals
  scope Scope.!! name


-- | Lookup a variable for writing
-- throws an error if its immutable
lookupVariableMutable :: (MonadState ProgramEnv m, MonadError ResultFailed m) => LocalVariable -> m (Typed Operand)
lookupVariableMutable name = do
  scope <- gets locals
  var <- scope Scope.!! name
  case var of
    Variable Mutable ty -> pure ty
    Variable Frozen  _  -> throwError ImmutableVariableError $ pshow name


lookupFunction :: (MonadState ProgramEnv m, MonadError ResultFailed m) => FunctionName -> m (Typed Operand)
lookupFunction name =
  gets funcs >>= (\case
    Nothing -> throwError UnknownFunctionError $ pshow name
    Just ty -> pure ty
    ) . Map.lookup name


addFunction :: (MonadState ProgramEnv m, MonadError ResultFailed m) => FunctionName -> Typed Operand -> m ()
addFunction func_name typed = do
  type_tracker <- gets types
  let all_types_in_func = TC.getContainedTypes $ type_ typed
  traverse_ (`TC.requireType` type_tracker) all_types_in_func

  program <- get
  if Map.member func_name (funcs program) then
    throwError DuplicateNameError $ pshow func_name
  else
    put $ program{funcs=Map.insert func_name typed (funcs program)}


addVariable :: (MonadState ProgramEnv m, MonadError ResultFailed m) => LocalVariable -> Variable -> m ()
addVariable var_name op = do
  program <- get
  new_locals <- Scope.insertUnique var_name op (locals program)
  put $ program{locals=new_locals}


type StateResult = ResultT (State ProgramEnv)
type LLVM = Module.ModuleBuilderT StateResult
type CodeGen = Module.IRBuilderT LLVM


withPosition :: MonadError ResultFailed m => Pos -> m a -> m a
withPosition pos = withError (\e -> e{errLoc=Just pos})

withFile :: MonadError ResultFailed m => FilePath -> m a -> m a
withFile file = withError (\e -> e{errFile=Just file})


-- | Get info on how this type is allocated
getAllocationType :: (MonadError ResultFailed m, MonadState ProgramEnv m) => AType -> m TC.Allocation
getAllocationType name = snd <$> lookupType' name
