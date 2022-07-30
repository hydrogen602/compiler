{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module Util.CompileResult where
import           Control.Monad.Identity     (Identity (Identity, runIdentity))
import           Control.Monad.State        (State, StateT (StateT), evalState,
                                             runState, runStateT, state,
                                             withState)
import qualified Control.Monad.State        as StateModule
import           Control.Monad.State.Class  (MonadState (get, put), gets,
                                             modify)
import           Control.Monad.Trans        (MonadTrans (lift))
import           Data.Functor               ((<&>))

import           Control.Monad.Trans.Except
import           Data.Bifunctor             (Bifunctor (first, second))

import           Control.Monad.Error.Class  (MonadError)
import qualified Control.Monad.Error.Class  as MError
import           Extras.PrettyShow          (PrettyShow (pshow))
import           Util.Util                  (ddot, (<.>))

data ResultFailed = ResultFailed {
    errType :: ErrorType,
    errMsg  :: String
  } deriving (Eq, Ord)

instance Show ResultFailed where
  show ResultFailed{errType=errType, errMsg=errMsg} =
    show errType ++ ": " ++ errMsg


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
  deriving (Show, Eq, Ord)

throwError :: MonadError ResultFailed m => ErrorType -> String -> m a
throwError = MError.throwError `ddot` ResultFailed

throwShowError :: (MonadError ResultFailed m, PrettyShow b) => ErrorType -> b -> m a
throwShowError errType = MError.throwError . ResultFailed errType . pshow

-- catchAll :: Applicative m => ResultT m a -> ResultT m (Maybe a)
-- catchAll re = undefined -- catchE

catch :: Monad m => ErrorType -> ResultT m a -> ResultT m (Maybe a)
catch err re = ExceptT $ runExceptT re <&> \case
  Left e  -> if errType e == err then Right Nothing else Left e
  Right a -> Right $ Just a

-- ResultT manipulation helpers

-- fromTransformer :: (m a -> b) -> ResultT m a -> Result b
-- fromTransformer runMonad = mapExceptT (Identity . runMonad)
--   where
--     f = runMonad

toTransformer :: Monad m => Result a -> ResultT m a
toTransformer = mapExceptT (pure . runIdentity)

fromMaybeResult :: Monad m => ResultT m a -> ResultT m (Maybe a) -> ResultT m a
fromMaybeResult throwable = (>>= maybe throwable pure)

fromMaybe :: Monad m => ResultT m a -> Maybe a -> ResultT m a
fromMaybe throwable = maybe throwable pure

mapInnerMonad :: (m1 (Either ResultFailed a) -> m2 (Either ResultFailed b)) -> ResultT m1 a -> ResultT m2 b
mapInnerMonad f = ExceptT . f . runExceptT

fromSuccess :: Monad m => ResultT m a -> m a
fromSuccess r = runExceptT r >>= \case
    Left rf -> error $ show rf
    Right a -> pure a


-- mapInnerMonad :: (m1 a -> m2 b) -> ResultT m1 a -> ResultT m2 b
-- mapInnerMonad mapFunc result = ResultT $ mapFunc (runResultT result)
--   where
--     x = mapFunc
--     func :: m1 (ResultBox a) -> m2 (ResultBox b)
--     func

-- Monad Specific
-- instance MonadState s (ResultT (State s)) where
--   get = ExceptT $ StateModule.gets Right
--   put = ExceptT . Right <.> StateModule.put


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
evalInner result init = ExceptT $ Identity x
  where
    x = evalState (runExceptT result) init

liftInner :: Result a -> ResultT (State s) a
liftInner result = ExceptT $ pure r
  where
    r = runIdentity $ runExceptT result
