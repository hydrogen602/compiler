{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Util.Types where

import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe, listToMaybe)
import qualified Data.Set               as Set

import           Control.Monad.Identity (Identity)
import           Extras.PrettyShow      (PrettyShow (..))
import           Types.Addon            (Typed)
import           Util.Classes           (Empty (empty), Nameable (..))
import           Util.Literals


newtype LocalVariable = LocalVariable {getLocalVariable :: String} deriving (Show, Eq, Ord) -- with type in the future
newtype FunctionName = FunctionName {getFunctionName :: String} deriving (Show, Eq, Ord)

instance Nameable LocalVariable where
  getName = getLocalVariable

instance Nameable FunctionName where
  getName = getFunctionName

instance PrettyShow FunctionName where
  pshow = getFunctionName


-- Consts

data Function f = Function {
  functionName :: FunctionName,
  params       :: [Typed LocalVariable],
  functionCode :: [Stmt f],
  literals     :: Literals2
} --deriving (Show, Eq, Ord)


-- Code

data UseNewLine = UseNewLine | NoUseNewLine deriving (Show, Eq, Ord)

data Stmt f =
    LetStmt (f LocalVariable) (f Expr)
  | AssignStmt (f LocalVariable) (f Expr)
  | PrintStmt UseNewLine (f Expr)
  | PrintLiteralStmt UseNewLine String
  | FuncCall FunctionName [f Expr]
  | IfStmt (f Expr) [Stmt f] [Stmt f]
  | WhileStmt (f Expr) [Stmt f]
  | ReturnStmt (f Expr)
  -- deriving (Eq, Ord)

deriving instance (Show (f LocalVariable), Show (f Expr)) => Show (Stmt f)

-- lets do preorder?
foldStmtr :: (Stmt f -> a -> a) -> a -> Stmt f -> a
foldStmtr f init stmt = aFinal
  where
    a = f stmt init
    aFinal = case stmt of
      IfStmt _ stmts1 stmts2 ->
        let a1 = foldr f a stmts1
        in foldr f a1 stmts2
      WhileStmt _ stmts      -> foldr f a stmts
      _                      -> a

foldStmtMap :: Monoid a => (Stmt f -> a) -> Stmt f -> a
foldStmtMap f = foldStmtr (flip (<>) . f) mempty



-- data Scope =
--   Scope
--     {
--       locals :: Set.Set LocalVariable,
--       code   :: [Stmt]
--     }
--   deriving (Show, Eq, Ord)

-- newScope :: Scope
-- newScope = Scope mempty mempty


data Program f = Program {
  functions :: Map.Map FunctionName (Function f),
  constants :: Consts,
  code      :: [Stmt f]
} --deriving (Show, Eq, Ord)

newProgram :: Program f
newProgram = Program mempty empty mempty


data Expr =
  Variabl LocalVariable |
  Immediate Int |
  Expr Op Expr Expr |
  FuncExpr FunctionName [Expr]
  deriving (Show, Eq, Ord)

data Op =
    ADD
  | LESS_THAN
  deriving (Show, Eq, Ord)
