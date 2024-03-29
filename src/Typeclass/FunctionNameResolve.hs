{-# LANGUAGE FlexibleContexts #-}
module Typeclass.FunctionNameResolve where
import           Data.Bifunctor            (Bifunctor (first), bimap)

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

-- | Attempts to do the opposite of getDotFunctionName
-- ArrayList_len -> (ArrayList, len)
breakApartFunctionName :: FunctionName -> Maybe (FunctionName, FunctionName)
breakApartFunctionName (FunctionName fName) = bimap FunctionName FunctionName <$> splitOnLast fName '_'

splitOnLast :: String -> Char -> Maybe (String, String)
splitOnLast "" _ = Nothing
splitOnLast (e:str) c
  | e == c = case result of
      Nothing    -> Just ([], str)
      Just split -> Just $ first (e:) split
  | otherwise = first (e:) <$> result
  where
    result = splitOnLast str c

