{-# LANGUAGE FlexibleContexts #-}
module Typeclass.FunctionNameResolve where

import           Control.Monad.Error.Class (MonadError)
import           Core.CompileResult        (ErrorType (TypeError), ResultFailed,
                                            throwError)
import           Core.Types                (FunctionName (FunctionName))
import           Types.Core                (AType, getTypeName)

-- | figure out the final function name
-- of something like ls.len()
-- for now, ls.len() => f"{typename(ls)}_len"
getDotFunctionName :: MonadError ResultFailed m => AType -> FunctionName -> m FunctionName
getDotFunctionName ty (FunctionName fName) =
  maybe
    (throwError TypeError $ "Can't use ." <> fName <> " on function type")
    (pure . FunctionName . (<> "_" <> fName))
    mTypeName
  where
    mTypeName = getTypeName ty



