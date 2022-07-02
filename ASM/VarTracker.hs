module ASM.VarTracker (
  ASMLabelTracker(all_labels),
  ASMVariableTrackerUnlimited(var_mapping)
  ) where

import           Control.Arrow       ((&&&))
import           Control.Monad.State (MonadTrans (lift), State, StateT (StateT),
                                      get, gets, modify, put)
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Numeric.Natural     (Natural)

import           ASM.Types
import           Data.Bifunctor      (Bifunctor (bimap))
import           Data.List           (isPrefixOf)
import qualified Util.Classes        as Classes
import qualified Util.Classes        as Nameable
import           Util.CompileResult  (ErrorType (InvalidVariableNameError, LabelConflictError),
                                      Result, throwError, throwNameError)
import qualified Util.Types          as Types


{-
If has 2 jumps - one to skip the if block, and one to skip the else block
While has 2 jumps - one to get to pre-condition check and one to break
-}

ifLabelPrefix = ("___else_block_", "___endif_")
whileLabelPrefix = ("__while_condition_", "__end_while_")

data ASMLabelTracker = ASMLabelTrackerConstructor {
  all_labels    :: Set.Set Label,

  -- counters (private)
  if_counter    :: Natural,
  while_counter :: Natural
}

instance Classes.Empty ASMLabelTracker where
  empty = ASMLabelTrackerConstructor mempty 0 0

-- addLabel' :: Label -> ASMLabelTracker -> Result ASMLabelTracker
-- addLabel' = undefined

-- getIfLabels' :: ASMLabelTracker -> (ASMLabelTracker, Label, Label)
-- getIfLabels' = undefined

-- getWhileLabels' :: ASMLabelTracker -> (ASMLabelTracker, Label, Label)
-- getWhileLabels' = undefined


addLabel :: Label -> StateT ASMLabelTracker Result ()
addLabel label
  | any (`isPrefixOf` Nameable.name label) [fst ifLabelPrefix, snd ifLabelPrefix, fst whileLabelPrefix, snd whileLabelPrefix] =
    lift $ throwError InvalidVariableNameError $ Nameable.name label
  | otherwise = do
    labels <- gets all_labels
    if label `elem` labels then
      lift $ throwError LabelConflictError $ Nameable.name label
    else
      modify (\tracker@ASMLabelTrackerConstructor{all_labels=l} -> tracker{all_labels=Set.insert label l})

getIfLabels :: State ASMLabelTracker (Label, Label)
getIfLabels = do
  if_num <- gets if_counter
  let
    f = Label . (++ show if_num)
    (labelA, labelB) = bimap f f ifLabelPrefix

  modify (
    \tracker@ASMLabelTrackerConstructor{if_counter=if_num, all_labels=l} ->
      tracker{if_counter=if_num+1, all_labels=Set.insert labelA $ Set.insert labelB l})
  pure (labelA, labelB)


getWhileLabels :: State ASMLabelTracker (Label, Label)
getWhileLabels = do
  while_num <- gets while_counter
  let
    f = Label . (++ show while_num)
    (labelA, labelB) = bimap f f whileLabelPrefix

  modify (
    \tracker@ASMLabelTrackerConstructor{while_counter=while_num, all_labels=l} ->
      tracker{while_counter=while_num+1, all_labels=Set.insert labelA $ Set.insert labelB l})
  pure (labelA, labelB)


data ASMVariableTrackerUnlimited = ASMVariableTrackerUnlimited {
  counter     :: Natural,
  var_mapping :: Map.Map Types.LocalVariable Natural
}

instance Classes.Empty ASMVariableTrackerUnlimited where
  empty = ASMVariableTrackerUnlimited 0 mempty


addVariable :: Types.LocalVariable -> StateT ASMVariableTrackerUnlimited Result UnlimitedRegister
addVariable var = do
  (varTracker, counter_val) <- gets (var_mapping &&& counter)
  if Map.member var varTracker then
    lift $ throwNameError $ Nameable.name var
  else do
    put $ ASMVariableTrackerUnlimited
      (counter_val + 1)
      $ Map.insert var counter_val varTracker
    pure $ UnlimitedRegister counter_val

getVariable :: Types.LocalVariable -> StateT ASMVariableTrackerUnlimited Result UnlimitedRegister
getVariable var = do
  varTracker <- gets var_mapping
  case Map.lookup var varTracker of
    Just s  -> pure $ UnlimitedRegister s
    Nothing -> lift $ throwNameError $ Nameable.name var



