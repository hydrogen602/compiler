{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Types where

import qualified Data.Map.Strict   as Map

import           Core.Classes      (Empty (empty), Nameable (..))
import           Core.Literals
import           Extras.Position   (Pos)
import           Extras.PrettyShow (PrettyShow (..))
import           Types.Addon       (Typed)


newtype LocalVariable = LocalVariable {getLocalVariable :: String} deriving (Show, Eq, Ord) -- with type in the future
newtype FunctionName = FunctionName {getFunctionName :: String} deriving (Show, Eq, Ord)

instance Nameable LocalVariable where
  getName = getLocalVariable

instance PrettyShow LocalVariable where
  pshow = getLocalVariable

instance Nameable FunctionName where
  getName = getFunctionName

instance PrettyShow FunctionName where
  pshow = getFunctionName


-- Consts

data Function f = Function {
  position     :: Pos,
  functionName :: FunctionName,
  params       :: [Typed LocalVariable],
  return_      :: Typed (),
  functionCode :: [Stmt f],
  literals     :: Literals2
}

deriving instance (Show (f (Stmt f)), Show (f (Expr f)), Show (f (Function f))) => Show (Function f)
deriving instance (Ord (f (Stmt f)), Ord (f (Expr f))) => Ord (Function f)
deriving instance (Eq (f (Stmt f)), Eq (f (Expr f))) => Eq (Function f)


-- Code

data Stmt f =
    LetStmt Pos LocalVariable (f (Expr f))
  | LetMutStmt Pos LocalVariable (f (Expr f))
  | AssignStmt Pos LocalVariable (f (Expr f))
  | WhileStmt (f (Expr f)) [Stmt f]
  | ReturnStmt (f (Expr f))
  | ExprStmt (f (Expr f))
  -- deriving (Eq, Ord)

deriving instance (Show (f (Expr f)), Show (f (Stmt f))) => Show (Stmt f)
deriving instance (Ord (f (Expr f))) => Ord (Stmt f)
deriving instance (Eq (f (Expr f))) => Eq (Stmt f)

-- lets do preorder?
foldStmtr :: (Stmt f -> a -> a) -> a -> Stmt f -> a
foldStmtr f initial stmt = aFinal
  where
    a = f stmt initial
    aFinal = case stmt of
      WhileStmt _ stmts -> foldr f a stmts
      _                 -> a

foldStmtMap :: Monoid a => (Stmt f -> a) -> Stmt f -> a
foldStmtMap f = foldStmtr (flip (<>) . f) mempty


data Program f = Program {
  functions :: Map.Map FunctionName (Function f),
  constants :: Consts,
  code      :: [Stmt f],
  filePath  :: FilePath
}

deriving instance (Show (Function f), Show (f (Stmt f)), Show (f (Expr f)), Show (f (Program f))) => Show (Program f)
deriving instance (Ord (f (Program f)), Ord (f (Stmt f)), Ord (f (Expr f))) => Ord (Program f)
deriving instance (Eq (f (Program f)), Eq (f (Stmt f)), Eq (f (Expr f))) => Eq (Program f)

newProgram :: FilePath -> Program f
newProgram = Program mempty empty mempty


data Expr f =
  Variabl LocalVariable |
  Immediate Int |
  Boolean Bool |
  Expr BinaryOp (f (Expr f)) (f (Expr f)) |
  Unary UnaryOp (f (Expr f)) |
  FuncExpr FunctionName [f (Expr f)] |
  DotFuncExpr FunctionName (f (Expr f)) [f (Expr f)] |
  IfExpr Pos (f (Expr f)) [Stmt f] [Stmt f]

deriving instance (Show (f (Stmt f)), Show (f (Expr f))) => Show (Expr f)
deriving instance Ord (f (Expr f)) => Ord (Expr f)
deriving instance Eq (f (Expr f)) => Eq (Expr f)


data BinaryOp =
    ADD
  | SUB
  | GREATER_THAN
  | GREATER_THAN_EQUAL
  | LESS_THAN
  | LESS_THAN_EQUAL
  | EQUAL
  | NOT_EQUAL
  | DIV
  | MOD
  | PROD
  deriving (Show, Eq, Ord)

data UnaryOp = NEG deriving (Show, Eq, Ord)
