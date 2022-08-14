module Core.AST where
import           Data.Either     (partitionEithers)
import qualified Data.Map        as Map

import           Core.Literals
import           Core.Types
import           Extras.Position (Pos)
import           Types.Addon     (Typed)

data AST f = AST {
  consts :: [Constant f],
  code   :: [Stmt f]
  }

data ASTFunction f = ASTFunction {
  position :: Pos,
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
-- getConstFromStmt (PrintLiteralStmt _ s) = singletonLiteral2 $ ConstValueStr s
getConstFromStmt _                      = mempty

astFunctionToFunction :: ASTFunction f -> Function f
astFunctionToFunction (ASTFunction pos name_ params_ ret_ code_) = Function pos name_ params_ ret_ code_ literals
  where
    literals = getLiteralsFromStmts code_

astToProgram :: FilePath -> AST f -> Program f
astToProgram file (AST consts_ code_) = Program functionMapping consts' code_ file
  where
    (constAssign, ast_funcs) = partitionEithers consts_
    funcs = map astFunctionToFunction ast_funcs
    functionMapping = Map.fromList $ map (\f -> (functionName f, f)) funcs

    allLiterals = getLiteralsFromStmts code_ <> foldMap literals funcs

    consts' = Consts (Map.fromList constAssign) allLiterals
