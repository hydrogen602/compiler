{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module Util.CompileResult where
import           Control.Monad.Identity    (Identity (Identity, runIdentity))
import           Control.Monad.State       (State, StateT (StateT), runStateT,
                                            state, withState)
import qualified Control.Monad.State       as StateModule
import           Control.Monad.State.Class (MonadState (get, put), gets, modify)
import           Control.Monad.Trans       (MonadTrans (lift))

import           Data.Bifunctor            (Bifunctor (first, second))
import           Util.Util                 (ddot)

data ResultBox a =
  ResultBoxFailed {
    errType :: ErrorType,
    errMsg  :: String
  }
  | ResultBox {
    value :: a
  } deriving (Eq, Ord)

instance Show (ResultBox a) where
  show ResultBoxFailed{errType=errType, errMsg=errMsg} =
    show errType ++ ": " ++ errMsg
  show ResultBox{value=value} = "Success"


newtype ResultT m a = ResultT { runResultT :: m (ResultBox a) } --deriving (Eq, Ord, Show)

instance Functor (ResultT m) where
  fmap f result = ResultT (f <$> runResultT result)

instance Applicative m => Applicative (ResultT m) where
  rmf <*> rma = ResultT $ runResultT rmf <*> runResultT rma
  pure = ResultT . pure

instance Monad m => Monad (ResultT m) where
  rma >>= f = ResultT $ do
    resultBox <- runResultT rma
    case resultBox of
      (ResultBox val) -> runResultT (f val)
      r               -> r

instance MonadTrans ResultT where
  lift = ResultT


type Result = ResultT Identity

data ErrorType =
    NameError
  | ArgumentError
  | UnexpectedError
  -- ^ for crashes that are due to internal issues with the compiler
  | LabelConflictError
  | InvalidVariableNameError
  | TypeError
  deriving (Show, Eq, Ord)

throwError :: ErrorType -> String -> ResultT m a
throwError = ResultTFailed

throwNameError :: String -> ResultT m a
throwNameError = ResultTFailed NameError

throwArgumentError :: String -> ResultT m a
throwArgumentError = ResultTFailed ArgumentError

throwUnexpectedError :: String -> ResultT m a
throwUnexpectedError = ResultTFailed UnexpectedError

catchAll :: Applicative m => ResultT m a -> ResultT m (Maybe a)
catchAll re = ResultT $ do
  box <- runResultT re
  pure $ case box of
    (ResultBox val)                  -> ResultBox $ Just val
    (ResultBoxFailed errType errMsg) -> ResultBox Nothing

catch :: Applicative m => ErrorType -> ResultT m a -> ResultT m (Maybe a)
catch err re = ResultT $ do
  box <- runResultT re
  pure $ case box of
    (ResultBox val)                  -> ResultBox $ Just val
    (ResultBoxFailed errType errMsg) ->
      if errType == err then ResultBox $ Nothing
      else ResultBoxFailed errType errMsg

-- ResultT manipulation helpers

fromTransformer :: (m a -> b) -> ResultT m a -> Result b
fromTransformer runMonad = mapInnerMonad (Identity . runMonad)

toTransformer :: Monad m => Result a -> ResultT m a
toTransformer = mapInnerMonad (pure . runIdentity)

runResultTEither :: ResultT m a -> m (Either (ErrorType, String) a)
runResultTEither (ResultT ma)                  = do
  box <- ma
  pure $ case box of
    (ResultBox a)                    -> Right a
    (ResultBoxFailed errType errMsg) -> Left (errType, errMsg)

fromMaybeResult :: Monad m => ResultT m a -> ResultT m (Maybe a) -> ResultT m a
fromMaybeResult throwable = (>>= maybe throwable pure)

fromMaybe :: Monad m => ResultT m a -> Maybe a -> ResultT m a
fromMaybe throwable = maybe throwable pure

-- mapInnerMonad :: (m1 a -> m2 b) -> ResultT m1 a -> ResultT m2 b
-- mapInnerMonad mapFunc result = ResultT $ mapFunc (runResultT result)
--   where
--     x = mapFunc
--     func :: m1 (ResultBox a) -> m2 (ResultBox b)
--     func


-- Monad Specific
instance MonadState s (ResultT (State s)) where
  get = ResultT $ StateModule.gets ResultBox
  put s = ResultT $ ResultBox <$> StateModule.put

-- firstState :: ResultT (State a) c -> a -> ResultT (State (a, b)) c
-- firstState input st = t
--   where
--     x = fromTransformer (`runState` st) input
--     t = toTransformer x

getFirst :: ResultT (State (a, b)) a
getFirst = gets fst

putFirst :: a -> ResultT (State (a, b)) ()
putFirst a = modify (\(_, b) -> (a, b))

firstState :: ResultT (State a) c -> ResultT (State (a, b)) c
firstState = mapInnerMonad $ state . uncurry . flip . flip withOther
  where
    stateModifer = runIdentity `ddot` runStateT
    withOther b = second (,b) `ddot` stateModifer

secondState :: ResultT (State b) c -> ResultT (State (a, b)) c
secondState = mapInnerMonad $ state . uncurry . flip withOther
  where
    stateModifer = runIdentity `ddot` runStateT
    withOther a = second (a,) `ddot` stateModifer

withStateResultT :: (s -> s) -> ResultT (State s) a -> ResultT (State s) a
withStateResultT = mapInnerMonad . withState

