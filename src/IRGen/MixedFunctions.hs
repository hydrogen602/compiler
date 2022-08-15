module IRGen.MixedFunctions where

import           LLVM.AST                   (Operand (ConstantOperand))
import qualified LLVM.AST.Constant          as C
import qualified LLVM.IRBuilder             as Module
import qualified LLVM.IRBuilder.Instruction as I
import           Prelude                    hiding (EQ)

import           Core.CompileResult         (ErrorType (TypeError), throwError)
import           Core.Types                 (BinaryOp (..))
import           Extras.PrettyShow          (PrettyShow (pshow))
import           IRGen.Types                (CodeGen)
import           LLVM.AST.IntegerPredicate  (IntegerPredicate (..))
import           Types.Addon                (Typed (Typed))
import qualified Types.Core                 as Ty

addition :: Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
addition = arithmeticOp I.add

subtraction :: Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
subtraction = arithmeticOp I.sub

multiplication :: Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
multiplication = arithmeticOp I.mul

division :: Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
division = arithmeticOp I.sdiv -- currently just quotient

modulo :: Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
modulo = arithmeticOp I.srem -- FIXME: this is the remainder, not the modulo

arithmeticOp :: (Operand -> Operand -> CodeGen Operand) -> Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
arithmeticOp llvmFunc (Typed ta a) (Typed tb b)
  | ta == Ty.i32 && tb == Ty.i32 = Typed Ty.i32 <$> llvmFunc a b
  | ta == Ty.i64 && tb == Ty.i64 = Typed Ty.i64 <$> llvmFunc a b
  | otherwise                    = throwError TypeError $ "Operation not supported for types" ++ pshow ta ++ " and " ++ pshow tb

negation :: Typed Operand -> CodeGen (Typed Operand)
negation (Typed ta a)
  | ta == Ty.i32 = Typed Ty.i32 <$> I.sub (ConstantOperand $ C.Int 32 0) a
  | ta == Ty.i64 = Typed Ty.i64 <$> I.sub (ConstantOperand $ C.Int 64 0) a
  | otherwise                    = throwError TypeError $ "Negation not supported for type" ++ pshow ta

lessThan :: Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
lessThan = compareOp SLT

greaterThan :: Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
greaterThan = compareOp SGT

greaterThanOrEqual :: Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
greaterThanOrEqual = compareOp SGE

lessThanOrEqual :: Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
lessThanOrEqual = compareOp SLE

equalTo :: Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
equalTo = compareOp EQ

notEqualTo :: Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
notEqualTo = compareOp NE

compareOp :: IntegerPredicate -> Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
compareOp predicate (Typed ta a) (Typed tb b)
  | ta == Ty.i32 && tb == Ty.i32 = Typed Ty.bool <$> I.icmp predicate a b
  | ta == Ty.i64 && tb == Ty.i64 = Typed Ty.bool <$> I.icmp predicate a b
  | otherwise                    = throwError TypeError err_msg
  where
    err_msg = pshow predicate <> " not supported for types" <> pshow ta <> " and " <> pshow tb

tryMatchComparison :: BinaryOp -> Maybe (Typed Operand -> Typed Operand -> CodeGen (Typed Operand))
tryMatchComparison GREATER_THAN       = Just greaterThan
tryMatchComparison GREATER_THAN_EQUAL = Just greaterThanOrEqual
tryMatchComparison LESS_THAN          = Just lessThan
tryMatchComparison LESS_THAN_EQUAL    = Just lessThanOrEqual
tryMatchComparison EQUAL              = Just equalTo
tryMatchComparison NOT_EQUAL          = Just notEqualTo
tryMatchComparison _                  = Nothing

tryMatchArithmetic :: BinaryOp -> Maybe (Typed Operand -> Typed Operand -> CodeGen (Typed Operand))
tryMatchArithmetic ADD  = Just addition
tryMatchArithmetic SUB  = Just subtraction
tryMatchArithmetic PROD = Just multiplication
tryMatchArithmetic DIV  = Just division
tryMatchArithmetic MOD  = Just modulo
tryMatchArithmetic _    = Nothing
