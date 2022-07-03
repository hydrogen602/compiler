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

data ResultT m a =
  ResultTFailed {
    errType :: ErrorType,
    errMsg  :: String
  }
  | ResultT {
    value :: m a
  }
  deriving (Eq, Ord)

instance Show (ResultT m a) where
  show ResultTFailed{errType=errType, errMsg=errMsg} =
    show errType ++ ": " ++ errMsg
  show ResultT{value=value} = "Success"


instance Functor m => Functor (ResultT m) where
  fmap _ (ResultTFailed errType errMsg) = ResultTFailed errType errMsg
  fmap f (ResultT value)                = ResultT $ fmap f value

instance Applicative m => Applicative (ResultT m) where
  (ResultT f) <*> (ResultT ma)         = ResultT (f <*> ma)
  (ResultTFailed errType errMsg) <*> _ = ResultTFailed errType errMsg
  _ <*> (ResultTFailed errType errMsg) = ResultTFailed errType errMsg

  pure = ResultT . pure

instance Monad m => Monad (ResultT m) where
  (ResultTFailed errType errMsg) >>= _ = ResultTFailed errType errMsg
  (ResultT mValue) >>= f               = ResultT undefined

  return = pure

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
catchAll (ResultT ma)        = ResultT (Just <$> ma)
catchAll (ResultTFailed _ _) = pure Nothing

catch :: Applicative m => ErrorType -> ResultT m a -> ResultT m (Maybe a)
catch errType (ResultT ma) = ResultT (Just <$> ma)
catch errType (ResultTFailed err msg)
  | errType == err = pure Nothing
  | otherwise = ResultTFailed err msg

-- ResultT manipulation helpers

fromTransformer :: (m a -> b) -> ResultT m a -> Result b
fromTransformer _ (ResultTFailed err_type err) = ResultTFailed err_type err
fromTransformer withMonad (ResultT m)          = pure $ withMonad m

toTransformer :: Monad m => Result a -> ResultT m a
toTransformer (ResultT a)                  = pure (runIdentity a)
toTransformer (ResultTFailed err_type err) = ResultTFailed err_type err

runResultT :: ResultT m a -> m a
runResultT (ResultT a)                  = a
runResultT (ResultTFailed err_type err) = error $ show (show err_type ++ ": " ++ err)

runResultTEither :: ResultT m a -> Either (ErrorType, String) (m a)
runResultTEither (ResultT a)                  = Right a
runResultTEither (ResultTFailed err_type err) = Left (err_type, err)

fromMaybeResult :: Monad m => ResultT m a -> ResultT m (Maybe a) -> ResultT m a
fromMaybeResult throwable = (>>= maybe throwable pure)

fromMaybe :: Monad m => ResultT m a -> Maybe a -> ResultT m a
fromMaybe throwable = maybe throwable pure

mapInnerMonad :: (m a -> m b) -> ResultT m a -> ResultT m b
mapInnerMonad mapFunc (ResultT st)      = ResultT $ mapFunc st
mapInnerMonad _ (ResultTFailed err msg) = ResultTFailed err msg


-- Monad Specific

instance MonadState s (ResultT (State s)) where
  get = ResultT StateModule.get -- what is this magic?
  put s = ResultT $ StateModule.put s

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
firstState (ResultT st) = ResultT $ state $ uncurry $ flip withOther
  where
    stateModifer = runIdentity . runStateT st
    withOther b = second (,b) . stateModifer
firstState (ResultTFailed err msg) = ResultTFailed err msg

secondState :: ResultT (State b) c -> ResultT (State (a, b)) c
secondState (ResultT st) = ResultT $ state $ uncurry withOther
  where
    stateModifer = runIdentity . runStateT st
    withOther a = second (a,) . stateModifer
secondState (ResultTFailed err msg) = ResultTFailed err msg

withStateResultT :: (s -> s) -> ResultT (State s) a -> ResultT (State s) a
withStateResultT = mapInnerMonad . withState

