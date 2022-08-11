module IRGen.Lib where

import           Data.Foldable  (traverse_)
import           LLVM.AST.Name  (mkName)
import qualified LLVM.IRBuilder as Module

import           Core.Types     (FunctionName (FunctionName))
import           IRGen.Types    (LLVM, addFunction, lookupType)
import           Types.Addon    (Typed (Typed))
import qualified Types.Core     as Ty


generateLib :: LLVM ()
generateLib = do
  let
    lib = [
      (Ty.FunctionType [Ty.i32] Ty.unit, "print"),
      (Ty.FunctionType [Ty.i32] Ty.unit, "println"),

      (Ty.FunctionType [] Ty.arrayList, "newArrayList"),
      (Ty.FunctionType [Ty.arrayList, Ty.i32] Ty.unit, "append"),
      (Ty.FunctionType [Ty.arrayList] Ty.i32, "pop"),
      (Ty.FunctionType [Ty.arrayList] Ty.i32, "len")
      ]

  traverse_ (\(ftype@(Ty.FunctionType args out), f_name) -> do
    args_t <- traverse lookupType args
    out_t <- lookupType out
    f <- Module.extern (mkName f_name) args_t out_t
    addFunction (FunctionName f_name) (Typed ftype f)
    ) lib
