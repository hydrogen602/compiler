{-# LANGUAGE TupleSections #-}

module Util.CompileResult where
import           Control.Monad.State (StateT (StateT))
import           Util.Util           (ddot)

type Result = Either (ErrorType, String)

data ErrorType =
    NameError
  | ArgumentError
  | UnexpectedError
  | LabelConflictError
  | InvalidVariableNameError
  deriving (Show, Eq, Ord)


throwError :: ErrorType -> String -> Result a
throwError = Left `ddot` (,)

throwNameError :: String -> Result a
throwNameError = Left . (NameError,)

throwArgumentError :: String -> Result a
throwArgumentError = Left . (ArgumentError,)

throwUnexpectedError :: String -> Result a
throwUnexpectedError = Left . (UnexpectedError,)

catchAllToMaybe :: Result a -> Result (Maybe a)
catchAllToMaybe (Right a) = pure $ Just a
catchAllToMaybe (Left _)  = pure Nothing

catchToMaybe :: ErrorType -> Result a -> Result (Maybe a)
catchToMaybe errType (Right a) = pure $ Just a
catchToMaybe errType (Left (err, msg))
  | errType == err = pure $ Nothing
  | otherwise = Left (err, msg)

catchAndPrint :: Result a -> IO (Maybe a)
catchAndPrint (Right a) = pure $ pure a
catchAndPrint (Left (err_type, err))  = print (show err_type ++ ": " ++ err) >> pure Nothing

fromSuccess :: Result a -> a
fromSuccess (Right a)              = a
fromSuccess (Left (err_type, err)) = error $ show (show err_type ++ ": " ++ err)
