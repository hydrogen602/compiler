{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module Core.CompileResult where

import           Control.Monad.Error.Class        (MonadError)
import qualified Control.Monad.Error.Class        as MError
import           Control.Monad.Identity           (Identity (Identity, runIdentity))
import           Control.Monad.State              (State, evalState, runState,
                                                   state, withState)
import           Control.Monad.State.Class        (gets, modify)
import           Control.Monad.Trans.Except
import           Data.Bifunctor                   (Bifunctor (second))

import           Compat.Control.Monad.Error.Class (withError)
import           Core.Util                        (ddot)
import           Extras.Debug                     (red)
import           Extras.Position                  (Pos (Pos))
import           Extras.PrettyShow                (PrettyShow (pshow))

data ResultFailed = ResultFailed {
    errFile :: Maybe FilePath,
    errLoc  :: Maybe Pos,
    errType :: ErrorType,
    errMsg  :: String
  } deriving (Eq, Ord, Show)

instance PrettyShow ResultFailed where
  pshow ResultFailed{errFile=eFile, errLoc=eLoc, errType=eType, errMsg=eMsg} =
    (case (eLoc, eFile) of
      (Nothing, Nothing)                 -> ""
      (Just loc, Nothing)                -> pshow loc
      (Nothing, Just file)               -> "(" ++ file ++ ") "
      (Just (Pos lns cols), Just file) ->
        "(" ++ file ++ ":" ++ show lns ++ ":" ++ show cols ++ ") "
    ) ++ show eType ++ ": " ++ eMsg


type ResultT = ExceptT ResultFailed
type Result = ResultT Identity


data ErrorType =
    UnknownVariableError
  | UnknownFunctionError
  | UnknownTypeError
  | DuplicateNameError
  | DuplicateTypeError
  | ArgumentError
  | UnexpectedError
  -- ^ for crashes that are due to internal issues with the compiler
  | LabelConflictError
  | InvalidVariableNameError
  | TypeError
  | ImmutableVariableError
  | DirectCallToDestroyError
  -- ^ if someone tries to call .destroy() directly. It gets called automatically upon going out of scope
  deriving (Show, Eq, Ord)

-- | Adds a message to an error, useful for adding context to errors based on where they happen
withContext :: MonadError ResultFailed m => String -> m a -> m a
withContext context = withError (\e -> e{errMsg=errMsg e ++ "\n" ++ context})

-- | Throws an error of type ResultFailed
throwError :: MonadError ResultFailed m => ErrorType -> String -> m a
throwError = MError.throwError `ddot` ResultFailed Nothing Nothing

throwShowError :: (MonadError ResultFailed m, PrettyShow b) => ErrorType -> b -> m a
throwShowError eType = MError.throwError . ResultFailed Nothing Nothing eType . pshow

-- | Catches only a specific error
catchError :: MonadError ResultFailed m => ErrorType -> m a -> m (Maybe a)
catchError errToCatch ma = MError.catchError (Just <$> ma) $
  \e@ResultFailed{errType=errT} ->
    if errT == errToCatch then
      pure Nothing
    else
      MError.throwError e

-- ResultT manipulation helpers

toTransformer :: Monad m => Result a -> ResultT m a
toTransformer = mapExceptT (pure . runIdentity)

fromMaybeResult :: Monad m => ResultT m a -> ResultT m (Maybe a) -> ResultT m a
fromMaybeResult throwable = (>>= maybe throwable pure)

fromMaybe :: MonadError ResultFailed m => m a -> Maybe a -> m a
fromMaybe throwable = maybe throwable pure

mapInnerMonad :: (m1 (Either ResultFailed a) -> m2 (Either ResultFailed b)) -> ResultT m1 a -> ResultT m2 b
mapInnerMonad f = ExceptT . f . runExceptT

fromSuccess :: Monad m => ResultT m a -> m a
fromSuccess r = runExceptT r >>= \case
    Left rf -> error $ red $ pshow rf
    Right a -> pure a


getFirst :: ResultT (State (a, b)) a
getFirst = gets fst

putFirst :: a -> ResultT (State (a, b)) ()
putFirst a = modify (\(_, b) -> (a, b))


firstState :: ResultT (State a) c -> ResultT (State (a, b)) c
firstState result = ExceptT $ state $ uncurry $ flip withOther
  where
    func = runState $ runExceptT result
    withOther b = second (,b) . func

secondState :: ResultT (State b) c -> ResultT (State (a, b)) c
secondState result = ExceptT $ state $ uncurry withOther
  where
    func = runState $ runExceptT result
    withOther a = second (a,) . func

withStateResultT :: (s -> s) -> ResultT (State s) a -> ResultT (State s) a
withStateResultT f = ExceptT . withState f . runExceptT


evalInner :: ResultT (State s) a -> s -> Result a
evalInner result init_state = ExceptT $ Identity x
  where
    x = evalState (runExceptT result) init_state

liftInner :: Result a -> ResultT (State s) a
liftInner result = ExceptT $ pure r
  where
    r = runIdentity $ runExceptT result
