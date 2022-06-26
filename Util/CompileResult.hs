module Util.CompileResult where


type CompileResult = Either ErrorType

data ErrorType = NameError String | ArgumentError String | UnexpectedError String deriving (Eq, Ord)

instance Show ErrorType where
    show (NameError e)       = "NameError: " ++ e
    show (ArgumentError e)   = "ArgumentError: " ++ e
    show (UnexpectedError e) = "UnexpectedError: " ++ e

throwError :: ErrorType -> CompileResult a
throwError = Left

throwNameError :: String -> CompileResult a
throwNameError = Left . NameError

throwArgumentError :: String -> CompileResult a
throwArgumentError = Left . ArgumentError

throwUnexpectedError :: String -> CompileResult a
throwUnexpectedError = Left . UnexpectedError

catchAndPrint :: CompileResult a -> IO (Maybe a)
catchAndPrint (Right a) = pure $ pure a
catchAndPrint (Left e)  = print e >> pure Nothing

fromSuccess :: CompileResult a -> a
fromSuccess (Right a) = a
fromSuccess (Left e)  = error $ show e
