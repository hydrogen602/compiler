{-# LANGUAGE LambdaCase #-}
module IRGen.Basics where

import           Data.Functor               (($>))
import           LLVM.AST                   (Name, Operand (ConstantOperand),
                                             mkName)
import qualified LLVM.AST.Constant          as C
import           LLVM.AST.IntegerPredicate  (IntegerPredicate (NE))
import qualified LLVM.IRBuilder.Instruction as I

import           Core.Classes               (Nameable (getName))
import           Core.Types                 (LocalVariable)
import           Extras.FixedAnnotated      (FixedAnnotated (getValue))
import           IRGen.Types                (CodeGen, Mutability (..),
                                             Variable (Variable), addVariable,
                                             lookupType, lookupVariable)
import           Types.Addon                (Typed (..), isType)
import           Types.Core                 (AType)
import qualified Types.Core                 as Ty



toLLVMName :: Nameable a => a -> Name
toLLVMName = mkName . getName


makeNewVar :: Mutability -> Typed Operand -> LocalVariable -> CodeGen (Typed Operand)
makeNewVar Frozen var lv =
  addVariable lv (Variable Frozen var) $> var
makeNewVar Mutable (Typed ty op) lv = do
  var <- createRawMutableVariable ty
  addVariable lv (Variable Mutable var)
  rawStoreInstruction (getValue var) op
  pure var


createRawMutableVariable :: AType -> CodeGen (Typed Operand)
createRawMutableVariable ty = do
  llvmType <- lookupType ty
  Typed ty <$> I.alloca llvmType Nothing 0

-- | Gets a mutable variable (pointer) and a value,
-- and stores the value in the variable
rawStoreInstruction :: Operand -> Operand -> CodeGen ()
rawStoreInstruction = flip I.store 0


getVarValue :: LocalVariable -> CodeGen (Typed Operand)
getVarValue name = lookupVariable name >>= \case
    Variable Frozen typed  -> pure typed
    Variable Mutable typed -> sequenceA $ flip I.load 0 <$> typed


toBool :: Typed Operand -> CodeGen (Typed Operand)
toBool cond =
  if cond `isType` Ty.bool then
    pure cond
  else
    Typed Ty.bool <$> I.icmp NE (getValue cond) (ConstantOperand $ C.Int 32 0)
