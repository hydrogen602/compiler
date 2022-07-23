module Util.AST where
import           Data.Either   (partitionEithers)
import qualified Data.Map      as Map
import           Data.Tree     (Tree)

import           Util.Literals
import           Util.Types

data AST = AST {
  consts :: [Constant],
  code   :: [Stmt]
  } deriving (Show, Eq, Ord)

data ASTFunction = ASTFunction {
  name     :: FunctionName,
  params   :: [LocalVariable] ,
  funcCode :: [Stmt]
  } deriving (Show, Eq, Ord)

type Constant = Either (ConstName, ConstValue) ASTFunction

-- Parse Helpers

startHelper :: Constant -> AST -> AST
startHelper c ast@AST{consts=cs} =
  ast{consts=c:cs}

fromStmts :: [Stmt] -> AST
fromStmts = AST []

-- Other

getLiteralsFromStmts :: [Stmt] -> Literals2
getLiteralsFromStmts = foldMap $ foldStmtMap getConstFromStmt

getConstFromStmt :: Stmt -> Literals2
getConstFromStmt (PrintLiteralStmt _ s) = singletonLiteral2 $ ConstValueStr s
getConstFromStmt _                      = mempty

astFunctionToFunction :: ASTFunction -> Function
astFunctionToFunction (ASTFunction name params code) = Function name params code literals
  where
    literals = getLiteralsFromStmts code

astToProgram :: AST -> Program
astToProgram (AST consts code) = Program functionMapping consts' code
  where
    (constAssign, ast_funcs) = partitionEithers consts
    funcs = map astFunctionToFunction ast_funcs
    functionMapping = Map.fromList $ map (\f -> (functionName f, f)) funcs

    allLiterals = getLiteralsFromStmts code <> foldMap literals funcs

    consts' = Consts (Map.fromList constAssign) allLiterals
