-- import           AST
import           Control.Monad.State
import           Data
import           Data.Char
import           Data.List            as List
import qualified Data.Map.Strict      as Map
import           Debug.Trace
import           Numeric.Natural
import           System.Environment   (getArgs)

import           ASM.CodeGen          ()
import           ASM.Translate        (translateMain)
import           CodeGen.Generator    (writeToFile, writeToStdout)

import           Control.Monad.Except (runExceptT)
import           Grammar
import           Translator
import           Util.AST             (astToProgram)
import           Util.Classes         (Empty (empty))
import           Util.CompileResult
import           Util.Flattened       (transformMany, transformProgram)
import           Util.FlattenedGen    ()
import           Util.Literals        (ConstValue (ConstValueStr), Consts (..))
import qualified Util.Types           as Types
import           Util.Util
import           Validator
import           Variable


--translate (ConstStmt name value) varTable = undefined

-- translateData :: Map.Map String ConstValue -> Error [AsmData]
-- translateData = traverse (uncurry checker) . Map.toList

--   where
--     checker :: String -> ConstValue -> Error AsmData
--     checker name constValue
--       | name == "main" = throwError "Cannot use label main"
--       | "if_end_" `isPrefixOf` name = throwError "Label cannot start with 'if_end_'"
--       | "else_end_" `isPrefixOf` name = throwError "Label cannot start with 'else_end_'"
--       | "while_" `isPrefixOf` name = throwError "Label cannot start with 'while_'"
--       | otherwise = case constValue of
--           ConstValueStr value -> pure $ AsmString name value
--           -- _                   -> throwError "Not supported"


-- translateFunc :: LabelPrefix -> [Function] -> [String] -> ([AsmData], [String])
-- translateFunc _ [] labels = ([], labels)
-- translateFunc prefix (CFunc name block args : ls) varTable
--   | name == "main" = error "Cannot use label main"
--   | "if_end_" `isPrefixOf` name = error "Label cannot start with 'if_end_'"
--   | "else_end_" `isPrefixOf` name = error "Label cannot start with 'else_end_'"
--   | "while_" `isPrefixOf` name = error "Label cannot start with 'while_'"
--   | "str_" `isPrefixOf` name = error "Label cannot start with 'str_'"
--   | name `elem` labelsNew = error $ "Label already used: " ++ name
--   | length args > 4 = error $ "Can only support a max of 4 arguments, got " ++ (show . length) args ++ " arguments"
--   | otherwise =
--     let varTabWithArgs = foldr (flip assignNewVar) (newVarTracker labelsNew) args
--         varMoves = concat $ zipWith (\arg aReg -> asmSetToRegister (getRegister arg varTabWithArgs) aReg) args allARegisters
--         (code, VariableTracker {stringLabels = labelsFinal}) = runState (translator name block) varTabWithArgs
--      in (AsmFunc name (varMoves ++ code) : aData, name : labelsFinal)
--   where
--     (aData, labelsNew) = translateFunc prefix ls varTable

-- printLiteralHelper :: Stmt -> State (Map.Map String ConstValue) Stmt
-- printLiteralHelper (IfStmt expr ifBlock elseBlock) = do
--   ifAst <- printLiteralProcessor ifBlock
--   elseAst <- printLiteralProcessor elseBlock
--   return (IfStmt expr ifAst elseAst)
-- printLiteralHelper (WhileStmt expr block) = do
--   whileAst <- printLiteralProcessor block
--   return (WhileStmt expr whileAst)
-- printLiteralHelper (PrintLiteralStmt nl s) =
--   state
--     ( \consts ->
--         let existsAlready :: Map.Map String ConstValue -> Maybe String
--             existsAlready mapping =
--               case fmap snd $ List.find (\x -> s == fst x) $ Map.toList mapping of
--                 Just (ConstValueStr label) -> Just label
--                 _                          -> Nothing

--             findFreeLabel :: Map.Map String ConstValue -> Int -> Int
--             findFreeLabel mapping n =
--               if already then
--                 findFreeLabel mapping (n+1)
--               else
--                 n
--               where
--                 already = Map.member ('s' : 't' : 'r' : '_' : show n) mapping

--          in case existsAlready consts of
--               (Just label) -> (PrintStmt nl (Variabl label), consts)
--               Nothing ->
--                 let label = "str_" ++ show (findFreeLabel consts 0)
--                  in (PrintStmt nl (Variabl label), Map.insert label (ConstValueStr s) consts)
--     )
-- printLiteralHelper ls = return ls

-- printLiteralProcessor :: [Stmt] -> State (Map.Map String ConstValue) [Stmt]
-- printLiteralProcessor [] = return []
-- printLiteralProcessor (st : stmts) = do
--   stNew <- printLiteralHelper st
--   stmtsFinal <- printLiteralProcessor stmts
--   return (stNew : stmtsFinal)

main :: IO ()
main = do
  args <- getArgs

  -- read input
  s <- case argumentExtract "-i" args of
    (Just inFile) -> readFile inFile
    Nothing       -> getContents

  let -- config output
    outFileName = case argumentExtract "-o" args of
      (Just outFile) -> outFile
      Nothing        -> "out.s"

    -- funcHelper :: Map.Map String ConstValue -> [Function] -> (Map.Map String ConstValue, [Function])
    -- funcHelper consts [] = (consts, [])
    -- funcHelper consts ((CFunc name stmts args) : funcs) =
    --   let (postConsts, postFuncs) = funcHelper consts funcs
    --       (finalStmts, finalConsts) = runState (printLiteralProcessor stmts) postConsts
    --    in --printLiteralProcessor postConsts stmts

    --       (finalConsts, CFunc name finalStmts args : postFuncs)

    -- parse
    ast = parser (s ++ "\n") -- verify .
    program = astToProgram ast
    p2 = evalState (transformProgram program) 0

  writeToFile p2 Nothing

  let
    asm = evalState (fromSuccess $ translateMain p2) (empty, empty)





  -- putStr "consts = "
  -- print consts
  -- putStr "ast = "
  -- print ast

  -- print ast
  -- putStrLn ""
  -- print program

  -- let s2 = evalState (transformProgram program) (0::Natural)

  -- print s2
  writeToStdout asm

  --let state = evalAST ast (LocalSt [])

  --putStrLn (show state)

  -- process consts
  -- let preAsmData = yeetError $ translateData consts
  -- let (asmFuncs, labels) = translateFunc "" funcs []

  --     asmData = asmFuncs ++ preAsmData

  --     varTable = newVarTracker labels

  -- putStrLn "==================================="
  -- print varTable
  -- putStrLn "==================================="
  -- -- translate ast into asm
  -- let (asm, table) = runState (translator "" ast) varTable

  -- print asmData

  -- let out = generateText (asm, asmData)

  --putStrLn out
  -- writeFile outFileName out
