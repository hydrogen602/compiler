module Util.Types where

import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe, listToMaybe)
import qualified Data.Set        as Set

import           Util.Classes    (Empty (empty), Nameable (..))
import           Util.Literals


newtype LocalVariable = LocalVariable {getLocalVariable :: String} deriving (Show, Eq, Ord) -- with type in the future
newtype FunctionName = FunctionName {getFunctionName :: String} deriving (Show, Eq, Ord)

instance Nameable LocalVariable where
  name = getLocalVariable

instance Nameable FunctionName where
  name = getFunctionName


-- Consts

data Function = Function {
  functionName :: FunctionName,
  params       :: [LocalVariable],
  functionCode :: [Stmt],
  literals     :: Literals2
} deriving (Show, Eq, Ord)


-- Code

data UseNewLine = UseNewLine | NoUseNewLine deriving (Show, Eq, Ord)

data Stmt =
    LetStmt LocalVariable Expr
  | AssignStmt LocalVariable Expr
  | PrintStmt UseNewLine Expr
  | PrintLiteralStmt UseNewLine String
  | FuncCall FunctionName [Expr]
  | IfStmt Expr [Stmt] [Stmt]
  | WhileStmt Expr [Stmt]
  | ReturnStmt Expr
  deriving (Show, Eq, Ord)

-- lets do preorder?
foldStmtr :: (Stmt -> a -> a) -> a -> Stmt -> a
foldStmtr f init stmt = aFinal
  where
    a = f stmt init
    aFinal = case stmt of
      IfStmt _ stmts1 stmts2 ->
        let a1 = foldr f a stmts1
        in foldr f a1 stmts2
      WhileStmt _ stmts      -> foldr f a stmts
      _                      -> a

foldStmtMap :: Monoid a => (Stmt -> a) -> Stmt -> a
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


data Program = Program {
  functions :: Map.Map FunctionName Function,
  constants :: Consts,
  code      :: [Stmt]
} deriving (Show, Eq, Ord)

newProgram :: Program
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
