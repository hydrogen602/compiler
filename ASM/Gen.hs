{-# LANGUAGE TupleSections #-}

module ASM.Gen where

import           Control.Arrow       ((&&&))
import           Control.Monad.State
import           Data.Foldable       (fold)
import           Data.List           (isPrefixOf)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import           Numeric.Natural

import           ASM.Types
import           ASM.VarTracker
import           Data.Bifunctor      (Bifunctor (bimap))
import           Util.Classes        (empty, name)
import           Util.CompileResult  (ErrorType (InvalidVariableNameError),
                                      Result, throwError)
import           Util.Flattened      (Stmt2 (..))
import qualified Util.Flattened      as Flattened
import qualified Util.Literals       as Literals


translateFunc :: Flattened.Function2 -> ASMFunc
translateFunc = undefined

translate :: Flattened.Program2 -> ASMProgram
translate (Flattened.Program2 funcs consts code) = undefined

translateCode :: [Flattened.Stmt2] -> Result [ASMLine]
translateCode code = evalStateT lines empty
  where
    lines = fold <$> traverse translate_one code
    translate_one :: Flattened.Stmt2 -> StateT ASMVariableTrackerUnlimited Result [ASMLine]
    translate_one (LetStmt var) = addVariable (Right var) >> pure []
    translate_one (AssignLiteralStmt var num) = do
      register <- getVariable var
      pure []
    translate_one _             = undefined

translateConsts :: Literals.Consts -> Result ASMData
translateConsts (Literals.Consts named (Literals.Literals2 literals)) =
  flip ASMData unnamed <$> named_converted
  where
    named_converted = Map.traverseWithKey convert_one_named named
    unnamed_converted = Map.fromList <$> traverse prep_lookup (Set.toList literals)
    unnamed = evalState unnamed_converted 0

    prep_lookup :: Literals.ConstValue -> State Natural (Literals.ConstValue, (Label, String))
    prep_lookup const_val = (const_val,) <$> convert_one_literal const_val
    -- prep_lookup const_val = ((const_val,) . fst &&& id) <$> convert_one_literal const_val

    convert_one_named :: Literals.ConstName -> Literals.ConstValue -> Result (Label, String)
    convert_one_named const_name const_value
      | any (`isPrefixOf` name const_name) specialPrefixes =
        throwError InvalidVariableNameError $ name const_name
      | otherwise = case const_value of
        Literals.ConstValueStr s -> pure (Label (name const_name), s)

    convert_one_literal :: Literals.ConstValue -> State Natural (Label, String)
    convert_one_literal const_value = do
      num <- get
      let
        l = Label $ literalConstPrefix ++ show num
        value = case const_value of
          Literals.ConstValueStr s -> s

      modify (+1)
      pure (l, value)
