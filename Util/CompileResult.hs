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

instance Semigroup m => Semigroup (CompileResult m) where
    (Success a) <> (Success b) = Success (a <> b)
    (Success _) <> err = err
    (NameError e) <> _ = NameError e
    (ArgumentError e) <> _ = ArgumentError e

instance Monoid m => Monoid (CompileResult m) where
    mempty = Success mempty
    mappend = (<>)
    -- mappend (Success a) (Success b) = Success $ mappend a b
    -- mappend (Success _) err = err
    -- mappend (NameError s) _ = NameError s
    -- mappend (ArgumentError s) _ = ArgumentError s

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