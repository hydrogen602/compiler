{-# LANGUAGE TupleSections #-}

module CodeGen.CodeWriter where
import           Control.Arrow                     (Arrow (first))
import qualified Control.Monad.Trans.Writer.Strict as Writer

newtype CodeWriter a = CodeWriter { runCodeWriter :: (a, [String]) } deriving (Show, Eq, Ord)

instance Functor CodeWriter where
  fmap f = CodeWriter . first f . runCodeWriter

instance Applicative CodeWriter where
  pure = CodeWriter . (,[])
  mf <*> ma = CodeWriter (f a, init <> w)
    where
      (f, init) = runCodeWriter mf
      (a, w) = runCodeWriter ma

instance Monad CodeWriter where
  ma >>= f = CodeWriter (b, w1 <> w2)
    where
      (a, w1) = runCodeWriter ma
      (b, w2) = runCodeWriter $ f a


withIdent :: Int -> CodeWriter a -> CodeWriter a
withIdent spaces writer = CodeWriter (a, map (indent++) w)
  where
    indent = replicate spaces ' '
    (a, w) = runCodeWriter writer

lines :: [String] -> CodeWriter ()
lines = CodeWriter . ((),)

oneLine :: String -> CodeWriter ()
oneLine = CodeWriter . ((),) . (:[])

newLine :: CodeWriter ()
newLine = CodeWriter ((), [""])


type LineWriter = Writer.Writer String

space :: LineWriter ()
space = Writer.writer ((), " ")

quote :: String -> LineWriter ()
quote s = Writer.writer ((), "\"" ++ s ++ "\"")
