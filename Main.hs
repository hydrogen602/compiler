
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

type LabelPrefix = String


asmDoStuffToRegisters :: Char -> (String -> String -> String -> [Line])
asmDoStuffToRegisters '+' = asmAddRegisters
asmDoStuffToRegisters '<' = asmLessThanRegisters
asmDoStuffToRegisters char = error $ "Unknown operator (registers): " ++ [char]

asmDoStuffImmediate :: Char -> (String -> String -> Int -> [Line])
asmDoStuffImmediate '+' = asmAddImmediate
asmDoStuffImmediate '<' = asmLessThanImmediate
asmDoStuffImmediate char = error $ "Unknown operator (immediate): " ++ [char]


-- no branches in expressions!!
expressionEvalHelper :: [TReg] -> Expr -> VariableTracker -> ([Line], String) -- returns lines and register where result is
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
expressionEval = expressionEvalHelper allTRegisters

expressionSetReg :: String -> Expr -> VariableTracker -> [Line]
expressionSetReg s expr varTable = fst $ expressionEvalHelper (s:allTRegisters) expr varTable


translator :: LabelPrefix -> [Stmt] -> VariableTracker -> ([Line], VariableTracker)
translator = --pref statements =
    let translate :: LabelPrefix -> Int -> Stmt -> VariableTracker -> ([Line], VariableTracker, Int)
        translate _ num (LetStmt name val) varTable = trace ("let stmt! name = " ++ name) $
            let varTableNew = assignNewVar varTable name
                register = getRegister name varTableNew
            in (case val of
                    (Variabl v) -> asmSetToRegister register (getRegister v varTableNew)
                    (Immediate n) -> asmSetToImmediate register n
                    expr@(Expr sym e1 e2) -> 
                        let code = expressionSetReg register expr varTableNew
                        in code, varTableNew, num)

        translate _ num (AssignStmt name val) varTable = 
            let register = getRegister name varTable
            in (case val of
                    (Variabl v) -> asmSetToRegister register (getRegister v varTable)
                    (Immediate n) -> asmSetToImmediate register n
                    expr@(Expr sym e1 e2) -> 
                        let code = expressionSetReg register expr varTable
                        in code, varTable, num)

        translate _ num (PrintStmt withNL (Variabl name)) varTable@VariableTracker{table=vars, stringLabels=labels} = 
            ((case getRegisterMaybe name varTable of
                (Just register) -> asmPrintReg register
                Nothing -> 
                    if name `elem` labels then asmPrintConstStr name 
                    else error $ "Cannot find variable: " ++ name

            ) ++ if withNL then printNewLineCall else [], varTable, num)


        translate _ num (PrintStmt withNL (Immediate n)) varTable = (asmPrintInt n ++ if withNL then printNewLineCall else [], varTable, num)

        translate _ num (PrintStmt withNL expr) varTable = 
            (let (code, reg) = expressionEval expr varTable
            in code ++ asmPrintReg reg ++ if withNL then printNewLineCall else [], varTable, num)

        translate prefix num (IfStmt expr block elseBlock) varTable@VariableTracker{table=vars, stringLabels=labels} = 
            let ifLabel = "if_end_" ++ prefix ++ '_':show num -- jump after if block
                elseLabel = "else_end_" ++ prefix ++ '_':show num -- jump after else block

                innerPrefix = (prefix ++ "_" ++ show num)

                (conditionCode, conditionReg) = expressionEval expr varTable
                (innerBlock, _) = translator innerPrefix block varTable

                ifCondition = EmptyLine:conditionCode ++ [Instruction "beq" [conditionReg, "$0", ifLabel]]
                afterIfBlock = [
                    EmptyLine,
                    Label ifLabel
                    ]

                code = case elseBlock of
                    [] -> ifCondition ++ innerBlock ++ afterIfBlock
                    block -> 
                        let (innerElseBlock, _) = translator innerPrefix elseBlock varTable
                            afterElseBlock = [EmptyLine, Label elseLabel]
                        in (ifCondition ++ innerBlock ++ [Instruction "j" [elseLabel]] ++ afterIfBlock ++ innerElseBlock ++ afterElseBlock)


            in (code, varTable, num+1) -- not passing varTableNew because scope
            -- increment num cause we just made some labels with that num

        translate _ _ (PrintLiteralStmt _ _) _ = error "Failed: PrintLiteralStmt should not appear in translate"

        translate prefix num (WhileStmt expr block) varTable@VariableTracker{table=vars, stringLabels=labels} =
            let loopLabel = "while_loop_" ++ prefix ++ '_':show num
                endLabel = "while_end_" ++ prefix ++ '_':show num

                innerPrefix = (prefix ++ "_" ++ show num)

                (conditionCode, conditionReg) = expressionEval expr varTable
                (innerBlock, _) = translator innerPrefix block varTable

                ifCondition = EmptyLine:Label loopLabel:conditionCode ++ [Instruction "beq" [conditionReg, "$0", endLabel]]

            in (ifCondition ++ innerBlock ++ [Instruction "j" [loopLabel], EmptyLine, Label endLabel], varTable, num+1)-- increment num cause we just made some labels with that num

        translate prefix num (FuncCall name) varTable =
            ([Instruction "jal" [name]], varTable, num)

        translate _ _ stmt _ = error $ "Failed on the statement: " ++ show stmt


        translatorHelper :: Int -> LabelPrefix -> [Stmt] -> VariableTracker -> ([Line], VariableTracker, Int)
        translatorHelper num prefix [] varTable = ([], varTable, num)
        translatorHelper num prefix (st:ls) varTable = --traceShow st $
            let (code, varTableNew, numNew) = translate prefix num st varTable
                (codeLater, varTableLater, numLast) = translatorHelper numNew prefix ls varTableNew
            in (code ++ codeLater, varTableLater, numLast)
    in removeLastOf3Tup `dddot` translatorHelper 0 --pref statements


--translate (ConstStmt name value) varTable = undefined 

translateData :: LabelPrefix -> [ConstStmt] -> [String] -> ([AsmData], [String])
translateData _ [] labels = ([], labels)
translateData prefix ((CStmtStr name value):ls) varTable
    | name == "main" = error "Cannot use label main"
    | "if_end_" `isPrefixOf` name = error "Label cannot start with 'if_end_'"
    | "else_end_" `isPrefixOf` name = error "Label cannot start with 'else_end_'"
    | "while_" `isPrefixOf` name = error "Label cannot start with 'while_'"
--    | "str_" `isPrefixOf` name = error "Label cannot start with 'str_'"
    | name `elem` labelsNew = error $ "Label already used: " ++ name
    | otherwise = (AsmString name value:aData, name:labelsNew)
    where (aData, labelsNew) = translateData prefix ls varTable

translateFunc :: LabelPrefix -> [Function] -> [String] -> ([AsmData], [String])
translateFunc _ [] labels = ([], labels)
translateFunc prefix (CFunc name block args:ls) varTable
    | name == "main" = error "Cannot use label main"
    | "if_end_" `isPrefixOf` name = error "Label cannot start with 'if_end_'"
    | "else_end_" `isPrefixOf` name = error "Label cannot start with 'else_end_'"
    | "while_" `isPrefixOf` name = error "Label cannot start with 'while_'"
    | "str_" `isPrefixOf` name = error "Label cannot start with 'str_'"
    | name `elem` labelsNew = error $ "Label already used: " ++ name
    | length args > 4 = error $ "Can only support a max of 4 arguments, got " ++ (show . length) args ++ " arguments"
    | otherwise = --(AsmString name value:aData, name:labels)
        let varTabWithArgs = foldr (flip assignNewVar) (newVarTracker labelsNew) args
            varMoves = concat $ zipWith (\arg aReg -> asmSetToRegister (getRegister arg varTabWithArgs) aReg) args allARegisters
            (code, VariableTracker{stringLabels=labelsFinal}) = translator name block varTabWithArgs
        in  (AsmFunc name (varMoves ++ code):aData, name:labelsFinal)
    where (aData, labelsNew) = translateFunc prefix ls varTable


printLiteralProcessor :: [ConstStmt] -> [Stmt] -> ([ConstStmt], [Stmt])
printLiteralProcessor consts [] = (consts, [])
printLiteralProcessor consts (st:stmts) =
    let printLiteralHelper :: [ConstStmt] -> Stmt -> ([ConstStmt], Stmt)

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
                --existsAlready (_:ls) = existsAlready ls -- ???

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


        (constsNew, stNew) = printLiteralHelper consts st
        (constsFinal, stmtsFinal) = printLiteralProcessor constsNew stmts
    in (constsFinal, stNew:stmtsFinal)


main :: IO ()
main = do
    args <- getArgs

    s <- case argumentExtract "-i" args of
        (Just inFile) -> readFile inFile
        Nothing  -> getContents 

    let outFileName = case argumentExtract "-o" args of
            (Just outFile) -> outFile
            Nothing -> "out.s"


        funcHelper :: [ConstStmt] -> [Function] -> ([ConstStmt], [Function])
        funcHelper consts [] = (consts, [])
        funcHelper consts ((CFunc name stmts args):funcs) = 
            let (postConsts, postFuncs) = funcHelper consts funcs
                (finalConsts, finalStmts) = printLiteralProcessor postConsts stmts

            in  (finalConsts, CFunc name finalStmts args:postFuncs) 


        (preConsts, preFuncs, preAst) = parser (s ++ "\n")
        (pre1consts, ast) = printLiteralProcessor preConsts preAst
        (consts, funcs) = funcHelper pre1consts preFuncs




    putStr "consts = "
    print consts
    putStr "ast = "
    print ast

    --let state = evalAST ast (LocalSt [])

    --putStrLn (show state)

    let (preAsmData, preLabels) = translateData "" consts []
    putStr "preLabels = "
    print preLabels
    let (asmFuncs, labels) = translateFunc "" funcs preLabels 

        asmData = asmFuncs ++ preAsmData

        varTable = newVarTracker labels

    putStrLn "==================================="
    print varTable
    putStrLn "==================================="
    --let registerTable = registerAssign ast dataLabels
    let (asm, table) = translator "" ast varTable

    print asmData
    
    let out = generateText (asm, asmData)

    --putStrLn out
    writeFile outFileName out

