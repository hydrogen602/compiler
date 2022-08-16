{-# LANGUAGE FlexibleContexts #-}
module Types.Checkers where
import           Control.Monad.Error.Class (MonadError)
import           Data.Foldable             (traverse_)

import           Core.CompileResult        (ErrorType (TypeError), ResultFailed,
                                            throwError)
import           Core.Types                (FunctionName)
import           Extras.FixedAnnotated     (FixedAnnotated (getAnnotation))
import           Extras.Misc               (strictZip)
import           Extras.PrettyShow         (PrettyShow (pshow))
import           Types.Addon               (MaybeTyped (..), Typed (..))
import           Types.Core


typeCheck :: MonadError ResultFailed m => AType -> Typed a -> m (Typed a)
typeCheck aType typed
  | aType == type_ typed = pure typed
  | otherwise = throwTypeError aType $ type_ typed

-- | type check
-- Note: be careful when passing in a monadic values as an argument
-- it is easy to apply the action twice.
-- A line like:
--    ty <- getAnnotation <$> typed
-- Will apply all monadic action/state changes
typeCheck' :: MonadError ResultFailed m => Maybe AType -> Typed a -> m (Typed a)
typeCheck' Nothing typed = pure typed
typeCheck' (Just aType) typed
  | aType == ty = pure typed
  | otherwise   = throwTypeError aType ty
  where
    ty = getAnnotation typed

typeCheck2 :: MonadError ResultFailed m => Typed a -> Typed a -> m AType
typeCheck2 (Typed t1 _) (Typed t2 _)
  | t1 == t2 = pure t1
  | otherwise = throwTypeError t1 t2

areTypesMatching :: Typed a -> Typed a -> Bool
areTypesMatching (Typed t1 _) (Typed t2 _) = t1 == t2

-- returns the function + the return type
typeCheckFunction :: MonadError ResultFailed m => Typed f -> [Typed a] -> FunctionName -> m (Typed f)
typeCheckFunction (Typed (FunctionType args ret) f) parameters f_name = do
  case strictZip args parameters of
    Just zipped -> traverse_ (uncurry typeCheck) zipped
    Nothing     -> throwError TypeError $ "Function argument count mismatch for " ++ pshow f_name

  pure $ Typed ret f
typeCheckFunction (Typed ty _) _ f_name = throwError TypeError $ "Expected function, but got " ++ pshow ty ++ " for " ++ pshow f_name


isCompatibleType :: MaybeTyped a -> AType -> Bool
isCompatibleType (MaybeTyped Nothing _)   = const True
isCompatibleType (MaybeTyped (Just ty) _) = (ty ==)


isType :: Typed a -> AType -> Bool
isType (Typed ty _) = (ty ==)
