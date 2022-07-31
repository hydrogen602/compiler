module Util.Resolver where

-- import           Data.Bifunctor (Bifunctor (second))
-- import           Types.Addon    (MaybeTyped, Typed)
-- import           Util.AST       (AST (AST), ASTFunction)
-- import           Util.Literals  (ConstName, ConstValue)

-- resolve :: AST UnresolvedTyped -> AST Typed
-- resolve (AST consts code) = AST (map resolveConsts consts) undefined

-- resolveConsts :: Either (ConstName, ConstValue) (ASTFunction UnresolvedTyped) -> Either (ConstName, ConstValue) (ASTFunction Typed)
-- resolveConsts = second resolveFunc
--   where
--     resolveFunc :: ASTFunction UnresolvedTyped -> ASTFunction Typed
--     resolveFunc = undefined
