{-# LANGUAGE OverloadedStrings #-}

module IRGen.CodeGenIR (generate) where

import           Control.Monad              (forM_)
import           Control.Monad.State.Strict (evalState)
import qualified Data.Map                   as Map
import qualified Data.Text.Lazy             as T
import           LLVM.AST                   hiding (Function, FunctionType,
                                             Instruction)
import qualified LLVM.AST.Constant          as C
import qualified LLVM.AST.Type              as Types
import qualified LLVM.IRBuilder             as Module
import qualified LLVM.IRBuilder.Instruction as I
import           LLVM.Pretty                (ppllvm)

import           Core.CompileResult         (fromSuccess)
import           Core.Types                 (Program (..))
import           IRGen.FunctionGen          (generateAllFunctions)
import           IRGen.Lib                  (generateLib)
import           IRGen.StatementGen         (generateStmt, withNewScopeAndDrop)
import           IRGen.Types                (LLVM, newProgramEnv, withFile)
import           Types.Addon                (MaybeTyped (..))


generate :: Program MaybeTyped -> T.Text
generate = ppllvm . generateModule


generateModule :: Program MaybeTyped -> Module
generateModule (Program func_mapping _ code' file) = evalState (fromSuccess result) newProgramEnv
  where
    funcs' :: LLVM ()
    funcs' = generateAllFunctions $ Map.elems func_mapping

    main :: LLVM Operand
    main = Module.function "main" [] Types.i32 $ \[] -> withNewScopeAndDrop $ do
      forM_ code' generateStmt
      I.ret $ ConstantOperand $ C.Int 32 0

    code_state = do
      generateLib
      funcs'
      main

    result = withFile file $ Module.buildModuleT "main" code_state
