module Util.AST where
import           Data.Either   (partitionEithers)
import qualified Data.Map      as Map
import           Data.Tree     (Tree)

import           Types.Addon   (Typed)
import           Util.Literals
import           Util.Types

data AST f = AST {
  consts :: [Constant f],
  code   :: [Stmt f]
  } --deriving (Show, Eq, Ord)

data ASTFunction f = ASTFunction {
  name     :: FunctionName,
  params   :: [Typed LocalVariable],
  ret      :: Typed (),
  funcCode :: [Stmt f]
  } --deriving (Show, Eq, Ord)

type Constant f = Either (ConstName, ConstValue) (ASTFunction f)

-- Parse Helpers

startHelper :: Constant f -> AST f -> AST f
startHelper c ast@AST{consts=cs} =
  ast{consts=c:cs}

fromStmts :: [Stmt f] -> AST f
fromStmts = AST []

-- Other

getLiteralsFromStmts :: [Stmt f] -> Literals2
getLiteralsFromStmts = foldMap $ foldStmtMap getConstFromStmt

getConstFromStmt :: Stmt f -> Literals2
getConstFromStmt (PrintLiteralStmt _ s) = singletonLiteral2 $ ConstValueStr s
getConstFromStmt _                      = mempty

astFunctionToFunction :: ASTFunction f -> Function f
astFunctionToFunction (ASTFunction name params ret code) = Function name params ret code literals
  where
    literals = getLiteralsFromStmts code

astToProgram :: AST f -> Program f
astToProgram (AST consts code) = Program functionMapping consts' code
  where
    (constAssign, ast_funcs) = partitionEithers consts
    funcs = map astFunctionToFunction ast_funcs
    functionMapping = Map.fromList $ map (\f -> (functionName f, f)) funcs

    allLiterals = getLiteralsFromStmts code <> foldMap literals funcs

    consts' = Consts (Map.fromList constAssign) allLiterals
