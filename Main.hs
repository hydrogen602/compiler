
import Data.Char
import Data.List
import System.Environment ( getArgs )
import Grammar
import Asm
import AST
import Util
import Data
import Variable
import Debug.Trace


asmDoStuffToRegisters :: Char -> (String -> String -> String -> [Line])
asmDoStuffToRegisters '+' = asmAddRegisters
asmDoStuffToRegisters '<' = asmLessThanRegisters
asmDoStuffToRegisters char = error $ "Unknown operator (registers): " ++ [char]

asmDoStuffImmediate :: Char -> (String -> String -> Int -> [Line])
asmDoStuffImmediate '+' = asmAddImmediate
asmDoStuffImmediate '<' = asmLessThanImmediate
asmDoStuffImmediate char = error $ "Unknown operator (immediate): " ++ [char]


-- no branches in expressions!!
expressionEvalHelper :: [TmpReg] -> Expr -> VariableTracker -> ([Line], String) -- returns lines and register where result is
expressionEvalHelper _ (Variabl s) varTable = 
    ([], getRegister s varTable)

expressionEvalHelper (register:_) (Immediate v) _ = 
    let code = asmSetToImmediate register v
    in (code, register)

expressionEvalHelper (tmp:_) (Expr sym (Variabl s) (Immediate n)) varTable = --trace ("bbbbb: " ++ s) $
    let reg = getRegister s varTable
    in  (asmDoStuffImmediate sym tmp reg n, tmp)

expressionEvalHelper (tmp:tmpRegLs) (Expr sym (Variabl s) e) varTable = trace ("aaaaa: " ++ s) $
    let register = getRegister s varTable
        (code, reg) = expressionEvalHelper tmpRegLs e varTable
        codeAdd = asmDoStuffToRegisters sym tmp register reg
    in (code ++ codeAdd, tmp)

expressionEvalHelper tmpRegLs@(tmp:_) (Expr sym (Immediate v) e) varTable = --trace ("immediate = " ++ show v) $
    let (code, reg) = expressionEvalHelper tmpRegLs e varTable
        (codeAdd, regOut) = (asmDoStuffImmediate sym tmp reg v, tmp)
    in  (code ++ codeAdd, regOut)

expressionEvalHelper (tmp:tmpRegLs) (Expr sym e1 e2) varTable = --trace "general expr eval helper" $
    let (code1, reg1) = expressionEvalHelper (tmp:tmpRegLs) e1 varTable
        (code2, reg2) = expressionEvalHelper tmpRegLs e2 varTable
        set2 = asmDoStuffToRegisters sym tmp reg1 reg2 -- bug probably?
    in (concat [code1, code2, set2], tmp)


expressionEvalHelper regs expr varTable = error $ "Expression not dealt with: expr = " ++ show expr

expressionEval :: Expr -> VariableTracker -> ([Line], String)
expressionEval = expressionEvalHelper tmpRegs

expressionSetReg :: String -> Expr -> VariableTracker -> [Line]
expressionSetReg s expr varTable = fst $ expressionEvalHelper (s:tmpRegs) expr varTable


translate :: Stmt -> VariableTracker -> ([Line], VariableTracker)
translate (LetStmt name val) varTable = trace ("let stmt! name = " ++ name) $
    let varTableNew = assignNewVar varTable name
        register = getRegister name varTableNew
    in (case val of
            (Variabl v) -> asmSetToRegister register (getRegister v varTableNew)
            (Immediate n) -> asmSetToImmediate register n
            expr@(Expr sym e1 e2) -> 
                let code = expressionSetReg register expr varTableNew
                in code, varTableNew)

translate (AssignStmt name val) varTable = 
    let register = getRegister name varTable
    in (case val of
            (Variabl v) -> asmSetToRegister register (getRegister v varTable)
            (Immediate n) -> asmSetToImmediate register n
            expr@(Expr sym e1 e2) -> 
                let code = expressionSetReg register expr varTable
                in code, varTable)

translate (PrintStmt withNL (Variabl name)) varTable@VariableTracker{table=vars, stringLabels=labels} = 
    ((case getRegisterMaybe name varTable of
        (Just register) -> asmPrintReg register
        Nothing -> 
            if name `elem` labels then asmPrintConstStr name 
            else error $ "Cannot find variable: " ++ name

    ) ++ if withNL then printNewLineCall else [], varTable)


translate (PrintStmt withNL (Immediate n)) varTable = (asmPrintInt n ++ if withNL then printNewLineCall else [], varTable)

translate (PrintStmt withNL expr) varTable = 
    (let (code, reg) = expressionEval expr varTable
    in code ++ asmPrintReg reg ++ if withNL then printNewLineCall else [], varTable)

translate (IfStmt expr block elseBlock) varTable@VariableTracker{table=vars, stringLabels=labels, ifLabelId=ifLabelNum} = 
    let ifLabel = "if_end_" ++ show ifLabelNum -- jump after if block
        elseLabel = "else_end_" ++ show ifLabelNum -- jump after else block
        
        (conditionCode, conditionReg) = expressionEval expr varTable
        (innerBlock, VariableTracker{ifLabelId=newIfLabelId}) = translator block (addToIfLabelId varTable 1)

        ifCondition = EmptyLine:conditionCode ++ [Instruction "beq" [conditionReg, "$0", ifLabel]]
        afterIfBlock = [
            EmptyLine,
            Label ifLabel
            ]
        
        (code, finalIfLabelId) = case elseBlock of
            [] -> (ifCondition ++ innerBlock ++ afterIfBlock, newIfLabelId)
            block -> 
                let (innerElseBlock, VariableTracker{ifLabelId=postElseLabelId}) = translator elseBlock (setToIfLabelId varTable newIfLabelId)
                    afterElseBlock = [EmptyLine, Label elseLabel]
                in (ifCondition ++ innerBlock ++ [Instruction "j" [elseLabel]] ++ afterIfBlock ++ innerElseBlock ++ afterElseBlock, postElseLabelId)
        
        
    in (code, setToIfLabelId varTable finalIfLabelId) -- not passing varTableNew because scope

translate (PrintLiteralStmt _ _) _ = error "Failed: PrintLiteralStmt should not appear in translate"

translate (WhileStmt expr block) varTable@VariableTracker{table=vars, stringLabels=labels, ifLabelId=ifLabelNum} =
    let loopLabel = "while_loop_" ++ show ifLabelNum
        endLabel = "while_end_" ++ show ifLabelNum

        (conditionCode, conditionReg) = expressionEval expr varTable
        (innerBlock, VariableTracker{ifLabelId=newIfLabelId}) = translator block (addToIfLabelId varTable 1)

        ifCondition = EmptyLine:Label loopLabel:conditionCode ++ [Instruction "beq" [conditionReg, "$0", endLabel]]

    in (ifCondition ++ innerBlock ++ [Instruction "j" [loopLabel], EmptyLine, Label endLabel], setToIfLabelId varTable newIfLabelId)

translate stmt varTable = error $ "Failed on the statement: " ++ show stmt


translator :: [Stmt] -> VariableTracker -> ([Line], VariableTracker)
translator [] varTable = ([], varTable)
translator (st:ls) varTable = --traceShow st $
    let (code, varTableNew) = translate st varTable
        (codeLater, varTableLater) = translator ls varTableNew
    in (code ++ codeLater, varTableLater)



--translate (ConstStmt name value) varTable = undefined 

translateData :: [ConstStmt] -> VariableTracker -> ([AsmData], [String], VariableTracker)
translateData [] varTable = ([], [], varTable)
translateData ((CStmtStr name value):ls) varTable
    | name == "main" = error "Cannot use label main"
    | "if_end_" `isPrefixOf` name = error "Label cannot start with 'if_end_'"
    | "else_end_" `isPrefixOf` name = error "Label cannot start with 'else_end_'"
    | "while_" `isPrefixOf` name = error "Label cannot start with 'while_'"
--    | "str_" `isPrefixOf` name = error "Label cannot start with 'str_'"
    | name `elem` labels = error $ "Label already used: " ++ name
    | otherwise = (AsmString name value:aData, name:labels, varTableNew)
    where (aData, labels, varTableNew) = translateData ls varTable

translateData (CFunc name block:ls) varTable
    | name == "main" = error "Cannot use label main"
    | "if_end_" `isPrefixOf` name = error "Label cannot start with 'if_end_'"
    | "else_end_" `isPrefixOf` name = error "Label cannot start with 'else_end_'"
    | "while_" `isPrefixOf` name = error "Label cannot start with 'while_'"
    | "str_" `isPrefixOf` name = error "Label cannot start with 'str_'"
    | name `elem` labels = error $ "Label already used: " ++ name
    | otherwise = --(AsmString name value:aData, name:labels)
        let --printLiteralProcessor 
            --AsmFunc name ([Line])
            (code, varTableFinal) = translator block (newVarTrackerWithId (stringLabels varTable) (ifLabelId varTable))
        in (AsmFunc name code:aData, name:labels, setToIfLabelId varTableFinal (ifLabelId varTableFinal))
    where (aData, labels, varTableNew) = translateData ls varTable


printLiteralHelper :: [ConstStmt] -> Stmt -> ([ConstStmt], Stmt)

printLiteralHelper consts (IfStmt expr ifBlock elseBlock) =
    let (ifConsts, ifAst) = printLiteralProcessor consts ifBlock
        (elseConsts, elseAst) = printLiteralProcessor ifConsts elseBlock
        --(IfAst, IfConsts)
    in  (elseConsts, IfStmt expr ifAst elseAst)  -- (stmt:ast, consts)

printLiteralHelper consts (WhileStmt expr block) =
    let (whileConsts, whileAst) = printLiteralProcessor consts block
    in  (whileConsts, WhileStmt expr whileAst)

printLiteralHelper consts (PrintLiteralStmt nl s) = 
    let existsAlready :: [ConstStmt] -> Maybe String
        existsAlready [] = Nothing  
        existsAlready (CStmtStr labelName str:ls) =
            if str == s then Just labelName
            else existsAlready ls
        existsAlready (_:ls) = existsAlready ls -- ???
        
        findFreeLabel :: [ConstStmt] -> Int
        findFreeLabel [] = 0
        findFreeLabel (CStmtStr ('s':'t':'r':'_':num) _:ls) =
            let n = 1 + read num :: Int
            in max n (findFreeLabel ls)
        findFreeLabel (_:ls) = findFreeLabel ls

    in case existsAlready consts of 
        (Just label) -> (consts, PrintStmt nl (Variabl label)) 
        Nothing -> 
            let label = "str_" ++ show (findFreeLabel consts)
            in (CStmtStr label s:consts, PrintStmt nl (Variabl label))

printLiteralHelper consts st = (consts, st)


printLiteralProcessor :: [ConstStmt] -> [Stmt] -> ([ConstStmt], [Stmt])
printLiteralProcessor consts [] = (consts, [])
printLiteralProcessor consts (st:stmts) =
    let (constsNew, stNew) = printLiteralHelper consts st
        (constsFinal, stmtsFinal) = printLiteralProcessor constsNew stmts
    in (constsFinal, stNew:stmtsFinal)



-- printLiteralProcessorForFunctions :: [ConstStmt] -> [ConstStmt] -> [ConstStmt]
-- printLiteralProcessorForFunctions _ [] = []
-- printLiteralProcessorForFunctions allConsts (CFunc name stmts:ls) = 
--     let (constsNew, stmtsFinal) = printLiteralProcessor allConsts stmts
--         constsFinal = printLiteralProcessorForFunctions constsNew ls

--     in  



main :: IO ()
main = do
    args <- getArgs

    s <- case argumentExtract "-i" args of
        (Just inFile) -> readFile inFile
        Nothing  -> getContents 
    
    let outFileName = case argumentExtract "-o" args of
            (Just outFile) -> outFile
            Nothing -> "out.s"

        (preConsts, preAst) = parser (s ++ "\n")
        (consts, ast) = printLiteralProcessor preConsts preAst



    print consts
    print ast

    --let state = evalAST ast (LocalSt [])

    --putStrLn (show state)

    let (asmData, dataLabels, varTable) = translateData consts (newVarTracker dataLabels)

    --let registerTable = registerAssign ast dataLabels
        (asm, table) = translator ast varTable

    print asmData
    let out = generateText (asm, asmData)

    --putStrLn out
    writeFile outFileName out

