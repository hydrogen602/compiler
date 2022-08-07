module Compat.Control.Monad.Error.Class where

import           Control.Monad.Error.Class (MonadError, catchError, throwError)

-- From mtl-2.3: Monad classes for transformers, using functional dependencies
-- But because llvm-hs-pure hasn't been updated in 3 years, I'm stuck with mtl-2.2

-- | 'MonadError' analogue to the 'Control.Exception.try' function.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

-- | 'MonadError' analogue to the 'withExceptT' function.
-- Modify the value (but not the type) of an error.  The type is
-- fixed because of the functional dependency @m -> e@.  If you need
-- to change the type of @e@ use 'mapError'.
withError :: MonadError e m => (e -> e) -> m a -> m a
withError f action = tryError action >>= either (throwError . f) pure
