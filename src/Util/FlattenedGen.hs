module Util.FlattenedGen () where


import           Control.Monad.Trans.Writer.Strict
import qualified Data.Map.Strict                   as Map

import           Data.Foldable
import           Data.List                         (intersperse)

import           CodeGen.Classes                   (CodeGeneratable (..))
import           Token                             (Token (NewLine))
import           Util.Classes                      (Nameable (..))
import           Util.Flattened
import           Util.Literals                     (ConstValue (..),
                                                    Consts (..))
import           Util.Types                        (Op (..),
                                                    UseNewLine (NoUseNewLine, UseNewLine))

oneLine = tell . (:[])


convertOp :: Op -> String
convertOp ADD       = "+"
convertOp LESS_THAN = "<"


quote :: String -> String
quote s = "\"" ++ s ++ "\""


commaSeperateVars :: Nameable a => [a] -> [Char]
commaSeperateVars = fold . intersperse ", " . map getName


convertOneStmt2 :: Stmt2 -> Writer [String] ()
convertOneStmt2 stmt2 = case stmt2 of
  LetStmt lv                -> pure ()
  AssignLiteralStmt e n     -> oneLine $ getName e ++ " = " ++ show n
  AssignStmt e e'           -> oneLine $ getName e ++ " = " ++ getName e'
  BinaryFuncStmt op e e' e2 -> oneLine $ getName e ++ " = " ++ getName e' ++ convertOp op ++ getName e2
  PrintStmt NoUseNewLine e           -> oneLine $ "process.stdout.write(" ++ getName e ++ ")"
  PrintStmt UseNewLine e           -> oneLine $ "console.log(" ++ getName e ++ ")"
  PrintLiteralStmt NoUseNewLine str  -> oneLine $ "process.stdout.write(" ++ quote str ++ ")"
  PrintLiteralStmt UseNewLine str  -> oneLine $ "console.log(" ++ quote str ++ ")"
  FuncCall fn es (Just var)        -> oneLine $ getName var ++ " = " ++ getName fn ++ "(" ++ commaSeperateVars es ++ ")"
  FuncCall fn es Nothing -> oneLine $ getName fn ++ "(" ++ commaSeperateVars es ++ ")"
  IfStmt e sts sts'         -> do
    oneLine $ "if (" ++ getName e ++ ") {"
    traverse_ convertOneStmt2 sts
    oneLine "} else {"
    traverse_ convertOneStmt2 sts'
    oneLine "}"
  WhileStmt e sts           -> do
    oneLine $ "while (" ++ getName e ++ ") {"
    traverse_ convertOneStmt2 sts
    oneLine "}"
  ReturnStmt e              -> oneLine $ "return " ++ getName e

generateFunction :: Function2 -> Writer [String] ()
generateFunction (Function2 func_name params code _) = do
  oneLine $ "function " ++ getName func_name ++ "(" ++ commaSeperateVars params ++ ") {"
  traverse_ convertOneStmt2 code
  oneLine "}"
  oneLine ""


generateCode :: Program2 -> Writer [String] ()
generateCode (Program2 funcs consts code) = do
  let
    (Consts named _) = consts

  Map.traverseWithKey (\const_name (ConstValueStr s) ->
    oneLine $ "const " ++ getName const_name ++ " = " ++ quote s) named

  oneLine ""

  traverse_ generateFunction funcs

  traverse_ convertOneStmt2 code


instance CodeGeneratable Program2 where
  targetName _ = "general-intermediate"
  fileEnding _ = "js"
  generate = execWriter . generateCode
