module IRGen.Basics where

import           LLVM.AST                   (Name, Operand (LocalReference),
                                             mkName)
import qualified LLVM.AST.Type              as Types
import qualified LLVM.IRBuilder.Instruction as I

import           Core.Classes               (Nameable (getName))
import           Core.Types                 (LocalVariable)
import           Extras.FixedAnnotated      (FixedAnnotated (getValue))
import           IRGen.Types                (CodeGen, Mutability (..),
                                             Variable (Variable), addVariable,
                                             lookupType)
import           Types.Addon                (Typed (..))


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
