module IRGen.Basics where

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
import qualified Types.Core                 as Ty



toLLVMName :: Nameable a => a -> Name
toLLVMName = mkName . getName


makeNewVar :: Mutability -> Typed Operand -> LocalVariable -> CodeGen (Typed Operand)
makeNewVar Frozen var lv = do
  addVariable lv (Variable Frozen var)
  pure var
makeNewVar Mutable (Typed ty op) lv = do
  llvmType <- lookupType ty
  var <- Typed ty <$> I.alloca llvmType Nothing 0
  addVariable lv (Variable Mutable var)
  I.store (getValue var) 0 op
  pure var


getVarValue :: LocalVariable -> CodeGen (Typed Operand)
getVarValue name = do
  var <- lookupVariable name
  case var of
    Variable Frozen typed -> pure typed
    Variable Mutable typed ->
      sequenceA $ flip I.load 0 <$> typed


toBool :: Typed Operand -> CodeGen (Typed Operand)
toBool cond =
  if cond `isType` Ty.bool then
    pure cond
  else
    Typed Ty.bool <$> I.icmp NE (getValue cond) (ConstantOperand $ C.Int 32 0)
