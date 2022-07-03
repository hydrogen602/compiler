module ASM.VarTracker (
  ASMLabelTracker(all_labels),
  ASMVariableTrackerUnlimited(var_mapping),
  addLabel,
  getIfLabels,
  getWhileLabels,
  newScope,
  addVariable,
  getVariable,
  specialPrefixes,
  literalConstPrefix
  ) where

import           Control.Arrow       ((&&&))
import           Control.Monad.State (MonadTrans (lift), State, StateT (StateT),
                                      evalStateT, get, gets, modify, put)
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Numeric.Natural     (Natural)

import           ASM.Types
import           Data.Bifunctor      (Bifunctor (bimap))
import           Data.List           (isPrefixOf)
import qualified Util.Classes        as Classes
import qualified Util.Classes        as Nameable
import           Util.CompileResult  (ErrorType (InvalidVariableNameError, LabelConflictError),
                                      Result, throwError, throwNameError,
                                      throwUnexpectedError)
import qualified Util.Flattened      as Flattened
import qualified Util.Types          as Types


{-
If has 2 jumps - one to skip the if block, and one to skip the else block
While has 2 jumps - one to get to pre-condition check and one to break
-}

ifLabelPrefix = ("___else_block_", "___endif_")
whileLabelPrefix = ("__while_condition_", "__end_while_")
literalConstPrefix = "__literal_"

specialPrefixes = [
  fst ifLabelPrefix, snd ifLabelPrefix,
  fst whileLabelPrefix, snd whileLabelPrefix,
  literalConstPrefix
  ]

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
  | any (`isPrefixOf` Nameable.name label) specialPrefixes =
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

  modify $
    \tracker@ASMLabelTrackerConstructor{if_counter=if_num, all_labels=l} ->
      tracker{if_counter=if_num+1, all_labels=Set.insert labelA $ Set.insert labelB l}
  pure (labelA, labelB)


getWhileLabels :: State ASMLabelTracker (Label, Label)
getWhileLabels = do
  while_num <- gets while_counter
  let
    f = Label . (++ show while_num)
    (labelA, labelB) = bimap f f whileLabelPrefix

  modify $
    \tracker@ASMLabelTrackerConstructor{while_counter=while_num, all_labels=l} ->
      tracker{while_counter=while_num+1, all_labels=Set.insert labelA $ Set.insert labelB l}
  pure (labelA, labelB)


data ASMVariableTrackerUnlimited = ASMVariableTrackerUnlimited {
  counter      :: Natural,
  var_mapping  :: Map.Map Flattened.GeneralVariable Natural,
  parent_scope :: Maybe ASMVariableTrackerUnlimited
}

instance Classes.Empty ASMVariableTrackerUnlimited where
  empty = ASMVariableTrackerUnlimited 0 mempty Nothing

newScope :: ASMVariableTrackerUnlimited -> ASMVariableTrackerUnlimited
newScope varTracker = ASMVariableTrackerUnlimited (counter varTracker) mempty $ Just varTracker

addVariable :: Flattened.GeneralVariable -> StateT ASMVariableTrackerUnlimited Result UnlimitedRegister
addVariable var = do
  (varTracker, counter_val) <- gets $ var_mapping &&& counter
  if Map.member var varTracker then
    lift $ throwNameError $ Nameable.name var
  else do
    modify $ \varTrack -> varTrack{counter=counter_val+1, var_mapping=Map.insert var counter_val varTracker}

    pure $ UnlimitedRegister counter_val

-- this should not modify the state
getVariable :: Flattened.GeneralVariable -> StateT ASMVariableTrackerUnlimited Result UnlimitedRegister
getVariable var = do
  varTracker <- gets var_mapping
  case Map.lookup var varTracker of
    Just s  -> pure $ UnlimitedRegister s
    Nothing -> gets parent_scope >>=
      lift . maybe (throwNameError $ Nameable.name var) (evalStateT $ getVariable var)

getOrAddVariable :: Flattened.GeneralVariable -> StateT ASMVariableTrackerUnlimited Result UnlimitedRegister
getOrAddVariable var = do
  getRegister <- catchToMaybe

  throwUnexpectedError ""
