module IRGen.MixedFunctions where

import           LLVM.AST                   (Operand)
import qualified LLVM.IRBuilder.Instruction as I

import           Core.CompileResult         (ErrorType (TypeError), throwError)
import           Extras.PrettyShow          (PrettyShow (pshow))
import           IRGen.Types                (CodeGen)
import           LLVM.AST.IntegerPredicate  (IntegerPredicate (SLT))
import           Types.Addon                (Typed (Typed))
import qualified Types.Core                 as Ty

addition :: Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
addition (Typed ta a) (Typed tb b)
  | ta == Ty.i32 && tb == Ty.i32 = Typed Ty.i32 <$> I.add a b
  | ta == Ty.i64 && tb == Ty.i64 = Typed Ty.i64 <$> I.add a b
  | otherwise                    = throwError TypeError $ "Addition not supported for types" ++ pshow ta ++ " and " ++ pshow tb

lessThan :: Typed Operand -> Typed Operand -> CodeGen (Typed Operand)
lessThan (Typed ta a) (Typed tb b)
  | ta == Ty.i32 && tb == Ty.i32 = Typed Ty.i32 <$> I.icmp SLT a b
  | ta == Ty.i64 && tb == Ty.i64 = Typed Ty.i64 <$> I.icmp SLT a b
  | otherwise                    = throwError TypeError $ "Less than not supported for types" ++ pshow ta ++ " and " ++ pshow tb

