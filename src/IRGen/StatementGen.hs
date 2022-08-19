{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections     #-}

module IRGen.StatementGen where

import           Control.Applicative           (Alternative ((<|>)))
import           Control.Monad                 (join, unless, void, when, (>=>))
import           LLVM.AST                      hiding (Function, FunctionType,
                                                Instruction)
import qualified LLVM.AST.Constant             as C
import           LLVM.IRBuilder                (currentBlock)
import qualified LLVM.IRBuilder                as Module
import qualified LLVM.IRBuilder.Instruction    as I
import           LLVM.Prelude                  (fromMaybe, sequenceA_,
                                                traverse_)

import           Core.CompileResult            (ErrorType (DirectCallToDestroyError, UnexpectedError),
                                                throwError, withContext)
import qualified Core.CompileResult            as Result
import           Core.Types                    (Expr (..), FunctionName,
                                                LocalVariable, Stmt (..),
                                                UnaryOp (NEG))
import           Extras.FixedAnnotated         (FixedAnnotated (getValue))
import           Extras.Misc                   (safeLast)
import           Extras.Scope                  (DroppedScopes)
import           IRGen.Basics                  (getVarValue, makeNewVar, toBool)
import           IRGen.MixedFunctions          (negation, tryMatchArithmetic,
                                                tryMatchComparison)
import           IRGen.Types                   (CodeGen,
                                                Mutability (Frozen, Mutable),
                                                Variable (variable),
                                                getAllocationType,
                                                lookupFunction,
                                                lookupVariableMutable,
                                                withNewScope, withPosition)
import           Typeclass.Builtin             (destroy)
import           Typeclass.FunctionNameResolve (breakApartFunctionName,
                                                getDotFunctionName)
import           Types.Addon                   (MaybeTyped (..), Typed (..))
import           Types.Checkers                (typeCheck, typeCheck',
                                                typeCheck2, typeCheckFunction)
import qualified Types.Consts                  as Co
import           Types.Core                    (Allocation (Heap))
import qualified Types.Core                    as Ty


generateExpr :: MaybeTyped (Expr MaybeTyped) -> CodeGen (Typed Operand)
generateExpr (MaybeTyped maybeExprTy expr) = do
  expr_operand <- helper expr
  typeCheck' maybeExprTy expr_operand
  where
    helper :: Expr MaybeTyped -> CodeGen (Typed Operand)
    helper (Boolean True)         = pure Co.true
    helper (Boolean False)        = pure Co.false
    helper (Variabl name)         = getVarValue name
    helper (Immediate n)          =
      pure $ Typed Ty.i32 $ ConstantOperand $ C.Int 32 (fromIntegral n)
    helper (Expr op e1 e2)        = join $ f <*> asOperand1 <*> asOperand2
      where
        err = throwError UnexpectedError $ "Binary Operand not found: " <> show op
        f = Result.fromMaybe err $ tryMatchArithmetic op <|> tryMatchComparison op
        asOperand1 = generateExpr e1
        asOperand2 = generateExpr e2

    helper (FuncExpr fName parameters) = do
      when (Just destroy == (snd <$> breakApartFunctionName fName)) $
        throwError DirectCallToDestroyError "Direct calls to .destroy() are forbidden, it is called automatically when going out of scope"

      generateFunctionCall fName parameters Nothing
    helper (DotFuncExpr fName calledOn parameters) = do
      when (fName == destroy) $
        throwError DirectCallToDestroyError "Direct calls to .destroy() are forbidden, it is called automatically when going out of scope"

      calledOnExpr <- generateExpr calledOn
      finalFuncName <- getDotFunctionName (type_ calledOnExpr) fName
      generateFunctionCall finalFuncName parameters $ Just calledOnExpr
    helper (Unary NEG e) = generateExpr e >>= negation
    helper (IfExpr pos ex sts_then sts_else) = withPosition pos $ mdo
      cond <- generateExpr ex
      b <- toBool cond

      Module.condBr (getValue b) then_block else_block

      then_block <- Module.block `Module.named` "then"

      op_if <- fromMaybe Co.unit . safeLast <$> withNewScopeAndDrop (traverse generateStmt sts_then)
      then_block_final <- currentBlock
      checkForExit $ Module.br merge_block

      else_block <- Module.block `Module.named` "else"
      op_else <- fromMaybe Co.unit . safeLast <$> withNewScopeAndDrop (traverse generateStmt sts_else)
      else_block_final <- currentBlock
      checkForExit $ Module.br merge_block

      merge_block <- Module.block `Module.named` "merge"

      finalType <- typeCheck2 op_if op_else

      if finalType == Ty.unit then
        pure Co.unit
      else
        Typed finalType <$> I.phi [(getValue op_if, then_block_final), (getValue op_else, else_block_final)]


generateStmt :: Stmt MaybeTyped -> CodeGen (Typed Operand)
generateStmt = \case
  LetMutStmt pos lv ex        -> withPosition pos $ do
    val <- generateExpr ex
    -- see https://llvm.org/docs/LangRef.html#store-instruction
    Co.voidUnit $ makeNewVar Mutable val lv
  LetStmt pos lv ex           -> withPosition pos $ do
    val <- generateExpr ex
    Co.voidUnit $ makeNewVar Frozen val lv
  AssignStmt pos lv ex        -> withPosition pos $ do
    var <- lookupVariableMutable lv
    pre_val <- generateExpr ex
    val <- typeCheck (type_ var) pre_val
    I.store (getValue var) 0 (getValue val)
    pure Co.unit
  WhileStmt ex sts       -> mdo
    Module.br cond_block
    cond_block <- Module.block `Module.named` "while_condition"
    cond <- generateExpr ex
    b <- toBool cond

    Module.condBr (getValue b) while_body break_block

    while_body <- Module.block `Module.named` "while_body"
    void $ withNewScopeAndDrop $ traverse_ generateStmt sts
    Module.br cond_block

    break_block <- Module.block `Module.named` "while_break"
    pure Co.unit
  ReturnStmt ex          -> do
    val <- generateExpr ex
    sequenceA_ $ I.ret <$> val
    pure Co.unit
  ExprStmt ex -> do
    generateExpr ex


checkForExit :: CodeGen () -> CodeGen ()
checkForExit m = do
 check <- Module.hasTerminator
 unless check m


generateFunctionCall :: FunctionName
  -> [MaybeTyped (Expr MaybeTyped)]
  -> Maybe (Typed Operand)
  -> CodeGen (Typed Operand)
generateFunctionCall fName parameters mFirst = do
  func <- lookupFunction fName
  params_ops <- traverse generateExpr parameters
  let add_ops = maybe id (:) mFirst

  (Typed ret f) <- typeCheckFunction func (add_ops params_ops) fName
  fmap (Typed ret) $ I.call f $ map ((,[]) . getValue) (add_ops params_ops)


-- | Tries to call .destroy() on the variable if it is heap allocated
dropVarIfPossible :: Typed Operand -> CodeGen ()
dropVarIfPossible typedOp = do
  allocType <- getAllocationType $ type_ typedOp
  case allocType of
    Heap -> withContext "Boxed types need to have a .destroy() function" $ do
      fn <- getDotFunctionName (type_ typedOp) destroy
      void $ generateFunctionCall fn [] (Just typedOp)
    _    -> pure ()


dropAll :: (DroppedScopes LocalVariable Variable, a) -> CodeGen a
dropAll (dropped, a) = traverse_ (dropVarIfPossible . variable) dropped >> pure a


withNewScopeAndDrop :: CodeGen a -> CodeGen a
withNewScopeAndDrop = withNewScope >=> dropAll
