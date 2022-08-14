module Extras.Debug where

import           Control.Monad.State.Strict (MonadState, modify)
import           Debug.Trace


red :: String -> String
red s = "\x1b[31m" <> s <> "\x1b[0m"

tracePrefix :: String -> String -> a -> a
tracePrefix pre s = trace $ red pre <> ": " <> s

traceMonad :: MonadState s m => String -> m ()
traceMonad msg = modify (trace msg)
