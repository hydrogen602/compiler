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

catchAndPrint :: Result a -> IO (Maybe a)
catchAndPrint (Right a) = pure $ pure a
catchAndPrint (Left (err_type, err))  = print (show err_type ++ ": " ++ err) >> pure Nothing

fromSuccess :: Result a -> a
fromSuccess (Right a)              = a
fromSuccess (Left (err_type, err)) = error $ show (show err_type ++ ": " ++ err)
