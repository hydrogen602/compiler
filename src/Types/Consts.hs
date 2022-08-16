module Types.Consts where

import           Data.Functor      (($>))
import           LLVM.AST          (Operand (ConstantOperand))
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type     as LType

import           Types.Addon       (Typed (Typed))
import qualified Types.Core        as Ty

unit :: Typed Operand
unit = Typed Ty.unit $ ConstantOperand $ C.Null LType.void

-- | Like void from Data.Functor, but turns things into the unit operand
voidUnit :: Functor f => f a -> f (Typed Operand)
voidUnit arg = arg $> unit
