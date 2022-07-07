module Util.FlattenedGen () where


import           Control.Monad.Trans.Writer.Strict
import qualified Data.Map.Strict                   as Map

import           Data.Foldable
import           Data.List                         (intersperse)

import           CodeGen.Classes                   (CodeGeneratable (..))
import           Token                             (Token (NewLine))
import           Util.Classes                      (Nameable (name))
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
commaSeperateVars = fold . intersperse ", " . map name


convertOneStmt2 :: Stmt2 -> Writer [String] ()
convertOneStmt2 stmt2 = case stmt2 of
  LetStmt lv                -> pure ()
  AssignLiteralStmt e n     -> oneLine $ name e ++ " = " ++ show n
  AssignStmt e e'           -> oneLine $ name e ++ " = " ++ name e'
  BinaryFuncStmt op e e' e2 -> oneLine $ name e ++ " = " ++ name e' ++ convertOp op ++ name e2
  PrintStmt NoUseNewLine e           -> oneLine $ "process.stdout.write(" ++ name e ++ ")"
  PrintStmt UseNewLine e           -> oneLine $ "console.log(" ++ name e ++ ")"
  PrintLiteralStmt NoUseNewLine str  -> oneLine $ "process.stdout.write(" ++ quote str ++ ")"
  PrintLiteralStmt UseNewLine str  -> oneLine $ "console.log(" ++ quote str ++ ")"
  FuncCall fn es (Just var)        -> oneLine $ name var ++ " = " ++ name fn ++ "(" ++ commaSeperateVars es ++ ")"
  FuncCall fn es Nothing -> oneLine $ name fn ++ "(" ++ commaSeperateVars es ++ ")"
  IfStmt e sts sts'         -> do
    oneLine $ "if (" ++ name e ++ ") {"
    traverse_ convertOneStmt2 sts
    oneLine "} else {"
    traverse_ convertOneStmt2 sts'
    oneLine "}"
  WhileStmt e sts           -> do
    oneLine $ "while (" ++ name e ++ ") {"
    traverse_ convertOneStmt2 sts
    oneLine "}"
  ReturnStmt e              -> oneLine $ "return " ++ name e

generateFunction :: Function2 -> Writer [String] ()
generateFunction (Function2 func_name params code _) = do
  oneLine $ "function " ++ name func_name ++ "(" ++ commaSeperateVars params ++ ") {"
  traverse_ convertOneStmt2 code
  oneLine "}"
  oneLine ""


generateCode :: Program2 -> Writer [String] ()
generateCode (Program2 funcs consts code) = do
  let
    (Consts named _) = consts

  Map.traverseWithKey (\const_name (ConstValueStr s) ->
    oneLine $ "const " ++ name const_name ++ " = " ++ quote s) named

  oneLine ""

  traverse_ generateFunction funcs

  traverse_ convertOneStmt2 code


instance CodeGeneratable Program2 where
  targetName _ = "general-intermediate"
  fileEnding _ = "js"
  generate = execWriter . generateCode
