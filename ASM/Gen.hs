{-# LANGUAGE TupleSections #-}

module ASM.Gen where

import           Control.Arrow       ((&&&))
import           Control.Monad.State
import           Data.Bifunctor      (Bifunctor (bimap), first)
import           Data.Foldable       (fold)
import           Data.List           (isPrefixOf)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import           Numeric.Natural

import           ASM.Types
import           ASM.VarTracker
import           Util.Classes        (empty, name)
import           Util.CompileResult
import qualified Util.Flattened      as Flattened
import qualified Util.Literals       as Literals
import qualified Util.Types


translateFunc :: Flattened.Function2 -> ASMFunc
translateFunc = undefined

translate :: Flattened.Program2 -> ASMProgram
translate (Flattened.Program2 funcs consts code) = undefined

translateCode :: [Flattened.Stmt2] -> ASMData -> Map.Map Util.Types.FunctionName Label -> Maybe UnlimitedRegister -> ResultT (State (ASMVariableTrackerUnlimited, ASMLabelTracker)) [ASMLine]
translateCode code asmData funcNameMapping returnReg = lines
  -- fromTransformer (`evalState` empty) lines
  where
    lines = fold <$> traverse translate_one code

    pureOneInstr = pure . (:[]) . Left
    oneInstr = fmap $ (:[]) . Left

    translate_one :: Flattened.Stmt2 -> ResultT (State (ASMVariableTrackerUnlimited, ASMLabelTracker)) [ASMLine]
    translate_one (Flattened.LetStmt var) = firstState $ addVariable (Right var) >> pure []
    translate_one (Flattened.AssignLiteralStmt var num) = firstState $ do
      register <- getVariable var
      pureOneInstr $ Uniary SET register $ Right $ ASMLiteral num
    translate_one (Flattened.AssignStmt varToSet varToGet) = firstState $
      oneInstr $ Uniary SET
        <$> getVariable varToSet
        <*> (Left <$> getVariable varToGet)
    translate_one (Flattened.BinaryFuncStmt op varToSet var1 var2) = firstState $
      oneInstr $ Binary (opToASM op)
        <$> getVariable varToSet
        <*> getVariable var1
        <*> (Left <$> getVariable var2)
    translate_one (Flattened.PrintStmt nl var) = firstState $ do
      reg <- getVariable var
      pureOneInstr $ Builtin [reg] (PrintInt nl) Nothing
    translate_one (Flattened.PrintLiteralStmt nl strLiteral) = firstState $ do
      let lit = Literals.toLiteral strLiteral

      label <- fromMaybe
        (throwUnexpectedError $ "String literal not found in Map of literals: " ++ show lit)
        $ Map.lookup lit $ getUnnamedData asmData

      pure [
        Left $ LoadLabel OneTypeSpecialRegister label,
        Left $ Builtin [OneTypeSpecialRegister] (PrintString nl) Nothing
       ]
    translate_one (Flattened.FuncCall func_name args return_var) = firstState $
      oneInstr $ FuncCall
        <$> traverse getVariable args
        <*> fromMaybe (throwNameError $ name func_name)
          (Map.lookup func_name funcNameMapping)
        <*> traverse getVariable return_var
    translate_one (Flattened.IfStmt condition if_block else_block) = do
      cond_reg <- firstState $ getVariable condition
      (l1, l2) <- secondState $ lift getIfLabels

      let x = withState (first newScope) (translateCode if_block asmData funcNameMapping Nothing)

      pureOneInstr $ IfStmt cond_reg ([Instruction]) l1 ([Instruction]) l2
    translate_one (Flattened.WhileStmt condition block) = undefined
    translate_one (Flattened.ReturnStmt var) = firstState $ case returnReg of
      Nothing -> throwError TypeError "Illegal return statement"
      Just ur -> oneInstr $ Uniary SET ur . Left <$> getVariable var



translateConsts :: Literals.Consts -> Result ASMData
translateConsts (Literals.Consts named (Literals.Literals2 literals)) =
  flip ASMData unnamed <$> named_converted
  where
    named_converted = Map.traverseWithKey convert_one_named named
    unnamed_converted = Map.fromList <$> traverse prep_lookup (Set.toList literals)
    unnamed = evalState unnamed_converted 0

    prep_lookup :: Literals.ConstValue -> State Natural (Literals.ConstValue, Label)
    prep_lookup const_val = (const_val,) <$> convert_one_literal const_val
    -- prep_lookup const_val = ((const_val,) . fst &&& id) <$> convert_one_literal const_val

    convert_one_named :: Literals.ConstName -> Literals.ConstValue -> Result (Label, Literals.ConstValue)
    convert_one_named const_name const_value
      | any (`isPrefixOf` name const_name) specialPrefixes =
        throwError InvalidVariableNameError $ name const_name
      | otherwise = pure (Label (name const_name), const_value)

    convert_one_literal :: Literals.ConstValue -> State Natural Label
    convert_one_literal const_value = do
      num <- get
      modify (+1)
      pure $ Label $ literalConstPrefix ++ show num
