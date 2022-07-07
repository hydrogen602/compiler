{-# LANGUAGE TupleSections #-}

module ASM.Translate where

import           Control.Arrow       (Arrow (second), (&&&))
import           Control.Monad.State (MonadState (get), MonadTrans (lift),
                                      State, evalState, modify)
import           Data.Bifunctor      (Bifunctor (bimap), first)
import           Data.Foldable       (fold, toList)
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


mainFuncName :: Label
mainFuncName = Label "main"


translateFunc ::
     Flattened.Function2
  -> ASMData
  -> Map.Map Util.Types.FunctionName Label
  -> ResultT (State (ASMVariableTrackerUnlimited, ASMLabelTracker)) ASMFunc
translateFunc (Flattened.Function2 func_name params code _) asmData funcNameMapping = do
  func_label <- fromMaybe (throwUnexpectedError $ "Could not find own function name in " ++ name func_name) $ Map.lookup func_name funcNameMapping
  let result = OneTypeSpecialRegister

  (lines, param_regs) <- mapInnerMonad withTrackerScope $ do
    param_regs <- firstState $ traverse (addVariable . Right) params
    lines <- translateCode code asmData funcNameMapping $ Just result
    pure (lines, param_regs)

  pure $ ASMFunc func_label lines param_regs $ Just result


translateMain :: Flattened.Program2 -> ResultT (State (ASMVariableTrackerUnlimited, ASMLabelTracker)) ASMProgram
translateMain (Flattened.Program2 funcs consts code) = do
  let
    funcNameToLabel = Label . name
    funcNameMapping = Map.fromSet funcNameToLabel $ Map.keysSet funcs

    extraLiterals = foldMap ((:[]) . Flattened.literals) funcs

    -- the implicit main function is dealt a bit specially here
    -- so other functions can't refer to main
    mainFunc = Flattened.Function2
      (Util.Types.FunctionName $ name mainFuncName)
      [] code empty

    withMain = Map.insert (Flattened.functionName mainFunc) mainFunc funcs

  -- set up labels, main + all func ones
  secondState $
    addLabel mainFuncName >> traverse addLabel funcNameMapping

  asmData <- toTransformer $ translateConsts consts extraLiterals

  codeMap <- traverse (\f -> translateFunc f asmData funcNameMapping) withMain

  pure $ ASMProgram asmData $ Map.mapKeys funcNameToLabel codeMap


translateCode ::
     [Flattened.Stmt2]
  -> ASMData
  -> Map.Map Util.Types.FunctionName Label
  -> Maybe UnlimitedRegister
  -> ResultT (State (ASMVariableTrackerUnlimited, ASMLabelTracker)) [ASMLine]
translateCode code asmData funcNameMapping returnReg = lines
  -- fromTransformer (`evalState` empty) lines
  where
    lines = fold <$> traverse translate_one code

    pureOneInstr :: a -> ResultT (State c) [Either a b]
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
        <*> fromMaybe (throwError UnknownFunctionError $ name func_name)
          (Map.lookup func_name funcNameMapping)
        <*> traverse getVariable return_var
    translate_one (Flattened.IfStmt condition if_block else_block) = do
      cond_reg <- firstState $ getVariable condition
      (l1, l2) <- secondState $ lift getIfLabels

      if_lines <- mapInnerMonad withTrackerScope $
        translateCode if_block asmData funcNameMapping Nothing

      else_lines <- mapInnerMonad withTrackerScope $
        translateCode if_block asmData funcNameMapping Nothing

      pureOneInstr $ IfStmt cond_reg if_lines l1 else_lines l2
    translate_one (Flattened.WhileStmt condition block) = do
      cond_reg <- firstState $ getVariable condition
      (l1, l2) <- secondState $ lift getIfLabels

      lines <- mapInnerMonad withTrackerScope $
        translateCode block asmData funcNameMapping Nothing

      pureOneInstr $ WhileStmt l1 cond_reg lines l2
    translate_one (Flattened.ReturnStmt var) = firstState $ case returnReg of
      Nothing -> throwError TypeError "Illegal return statement"
      Just ur -> oneInstr $ Uniary SET ur . Left <$> getVariable var


translateConsts :: Literals.Consts -> [Literals.Literals2] -> Result ASMData
translateConsts (Literals.Consts named (Literals.Literals2 literals)) extraLiterals =
  flip ASMData unnamed <$> named_converted
  where
    extra_unnamed_sets = foldMap Literals.getLiterals2 extraLiterals
    unnamed_converted = Map.fromList <$> traverse prep_lookup (Set.toList $ literals <> extra_unnamed_sets)
    unnamed = evalState unnamed_converted 0

    named_converted = Map.traverseWithKey convert_one_named named

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


withTrackerScope :: State (ASMVariableTrackerUnlimited, ASMLabelTracker) a -> State (ASMVariableTrackerUnlimited, ASMLabelTracker) a
withTrackerScope action = do
  modify $ first newScope
  a <- action
  modify $ first popScope
  pure a
