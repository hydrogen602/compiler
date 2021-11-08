module Util.CompileResult where

-- let a be the output
data CompileResult a = Success a | NameError String | ArgumentError String deriving Show

instance Functor CompileResult where
    fmap f m = m >>= (return . f) 

instance Applicative CompileResult where
    pure = return
    (<*>) (Success f) = (>>= (return . f))
    (<*>) (NameError s)  = const (NameError s)
    (<*>) (ArgumentError s) = const (ArgumentError s)
    -- (<*>) (err) = const (err)

instance Monad CompileResult where
    Success a >>= f = f a
    NameError s >>= f = NameError s
    ArgumentError s >>= f = ArgumentError s

    return = Success

nameMaybeToCResult :: Maybe a -> String -> CompileResult a
nameMaybeToCResult (Just a) = const (Success a)
nameMaybeToCResult Nothing = NameError

assertSuccess :: CompileResult a -> a
assertSuccess (Success a) = a
assertSuccess (NameError s) = error $ "NameError: " ++ s
assertSuccess (ArgumentError s) = error $ "ArgumentError: " ++ s

assertSuccessPassThrough :: CompileResult () -> b -> b
assertSuccessPassThrough (Success _) b = b
assertSuccessPassThrough (NameError s) _ = error $ "NameError: " ++ s
assertSuccessPassThrough (ArgumentError s) _ = error $ "ArgumentError: " ++ s