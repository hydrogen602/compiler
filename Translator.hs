module Translator where
-- turn the AST into ASM
import Asm
import Util
import Data
import AST
import Control.Monad.State  
import Variable
import Debug.Trace
import Expression


translator :: LabelPrefix -> [Stmt] -> State VariableTracker [Line]
translator pref stmts = --pref statements =
    let translate :: LabelPrefix -> Stmt -> State (VariableTracker, Int) [Line]
        translate _ (LetStmt name expr) = do
            assignNewVar2 name
            (varTableNew, _) <- get
            let register = getRegister name varTableNew

            return (expressionSetReg register expr varTableNew)

        translate _ (AssignStmt name expr) = do
            (varTable, _) <- get
            let register = getRegister name varTable
            return (expressionSetReg register expr varTable)

        translate _ (PrintStmt withNL (Variabl name)) = do
            (varTable, num) <- get
            return $ (case getRegisterMaybe name varTable of
                    (Just register) -> asmPrintReg register
                    Nothing ->
                        if name `elem` stringLabels varTable then asmPrintConstStr name 
                        else error $ "Cannot find variable: " ++ name

                ) ++ if withNL then printNewLineCall else []


        translate _ (PrintStmt withNL (Immediate n)) = return $ asmPrintInt n ++ if withNL then printNewLineCall else []

        translate _ (PrintStmt withNL expr) = do
            (varTable, _) <- get
            let (code, reg) = expressionEval expr varTable
            return $ code ++ asmPrintReg reg ++ if withNL then printNewLineCall else []

        translate prefix (IfStmt expr block elseBlock) = state $ \(varTable, num) -> 
            let ifLabel = "if_end_" ++ prefix ++ '_':show num -- jump after if block
                elseLabel = "else_end_" ++ prefix ++ '_':show num -- jump after else block

                innerPrefix = (prefix ++ "_" ++ show num)

                (conditionCode, conditionReg) = expressionEval expr varTable
                (innerBlock, _) = runState (translator innerPrefix block) varTable

                ifCondition = EmptyLine:conditionCode ++ [Instruction "beq" [conditionReg, "$0", ifLabel]]
                afterIfBlock = [
                    EmptyLine,
                    Label ifLabel
                    ]

                code = case elseBlock of
                    [] -> ifCondition ++ innerBlock ++ afterIfBlock
                    block -> 
                        let (innerElseBlock, _) = runState (translator innerPrefix elseBlock) varTable
                            afterElseBlock = [EmptyLine, Label elseLabel]
                        in (ifCondition ++ innerBlock ++ [Instruction "j" [elseLabel]] ++ afterIfBlock ++ innerElseBlock ++ afterElseBlock)


            in (code, (varTable, num+1)) -- not passing varTableNew because scope
            -- increment num cause we just made some labels with that num

        translate _ (PrintLiteralStmt _ _) = error "Failed: PrintLiteralStmt should not appear in translate"

        translate prefix (WhileStmt expr block) = state $ \(varTable, num) ->
            let loopLabel = "while_loop_" ++ prefix ++ '_':show num
                endLabel = "while_end_" ++ prefix ++ '_':show num

                innerPrefix = (prefix ++ "_" ++ show num)

                (conditionCode, conditionReg) = expressionEval expr varTable
                (innerBlock, _) = runState (translator innerPrefix block) varTable

                ifCondition = EmptyLine:Label loopLabel:conditionCode ++ [Instruction "beq" [conditionReg, "$0", endLabel]]

            in (ifCondition ++ innerBlock ++ [Instruction "j" [loopLabel], EmptyLine, Label endLabel], (varTable, num+1))-- increment num cause we just made some labels with that num

        translate prefix (FuncCall name givenArgs) = state $ \(varTable, num) -> 
            let codeRegPairs = concat $ zipWith (\argExpr argReg -> expressionSetReg argReg argExpr varTable) givenArgs allARegisters 
                --varMoves = concat $ zipWith (\arg aReg -> asmSetToRegister aReg (getRegister arg varTable)) givenArgs allARegisters

            in (codeRegPairs ++ [Instruction "jal" [name]], (varTable, num))
        
        translate prefix (ReturnStmt var) = state $ \(varTable, num) -> 
            let m = asmSetToRegister "$v0" (getRegister var varTable)
            in (m, (varTable, num))

        -- translate _ _ stmt _ = error $ "Failed on the statement: " ++ show stmt


        translatorHelper :: LabelPrefix -> [Stmt] -> State (VariableTracker, Int) [Line]
        translatorHelper prefix [] = state $ \(varTable, num) -> ([], (varTable, num))
        translatorHelper prefix (st:ls) = do
            code <- translate prefix st
            codeLater <- translatorHelper prefix ls
            return (code ++ codeLater)
    
    in get >>= (\varTable ->
        let (lines, state2) = runState (translatorHelper pref stmts) (varTable, 0)
        in putAndReturn (fst state2) lines) --removeLastOf3Tup `dddot` translatorHelper 0 --pref statements
