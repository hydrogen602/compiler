
import Data.Char
import Control.Monad.State  
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

-- expressionEvalHelper tmpRegLs@(tmp:_) (Expr sym (Immediate v) e) varTable = --trace ("immediate = " ++ show v) $
--     let (code, reg) = expressionEvalHelper tmpRegLs e varTable
--         (codeAdd, regOut) = (asmDoStuffImmediate sym tmp reg v, tmp)
--     in  (code ++ codeAdd, regOut)

expressionEvalHelper (tmp:tmpRegLs) (Expr sym e1 e2) varTable = --trace "general expr eval helper" $
    let (code1, reg1) = expressionEvalHelper (tmp:tmpRegLs) e1 varTable
        (code2, reg2) = expressionEvalHelper tmpRegLs e2 varTable
        set2 = asmDoStuffToRegisters sym tmp reg1 reg2 -- bug probably?
    in (concat [code1, code2, set2], tmp)

expressionEvalHelper (tmp:tmpRegLs) (FuncExpr fName givenArgs) varTable =
    let codeRegPairs = concat $ zipWith (\argExpr argReg -> expressionSetReg argReg argExpr varTable) givenArgs allARegisters 
        
        fetch = asmSetToRegister tmp "$v0"
    in (codeRegPairs ++ (Instruction "jal" [fName]:fetch), tmp)

expressionEvalHelper regs expr varTable = error $ "Expression not dealt with: expr = " ++ show expr

expressionEval :: Expr -> VariableTracker -> ([Line], String)
expressionEval = expressionEvalHelper allTRegisters

expressionSetReg :: String -> Expr -> VariableTracker -> [Line]
expressionSetReg s (Variabl v) varTable = asmSetToRegister s (getRegister v varTable)
expressionSetReg s (Immediate n) varTable = asmSetToImmediate s n
expressionSetReg s expr varTable = fst $ expressionEvalHelper (s:allTRegisters) expr varTable


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
            (code, VariableTracker{stringLabels=labelsFinal}) = runState (translator name block) varTabWithArgs
        in  (AsmFunc name (varMoves ++ code):aData, name:labelsFinal)
    where (aData, labelsNew) = translateFunc prefix ls varTable


printLiteralHelper :: Stmt -> State [ConstStmt] Stmt
printLiteralHelper (IfStmt expr ifBlock elseBlock) = do
    ifAst <- printLiteralProcessor ifBlock
    elseAst <- printLiteralProcessor elseBlock
    return (IfStmt expr ifAst elseAst)

printLiteralHelper (WhileStmt expr block) = do
    whileAst <- printLiteralProcessor block
    return (WhileStmt expr whileAst)

printLiteralHelper (PrintLiteralStmt nl s) = state (\consts ->
    let existsAlready :: [ConstStmt] -> Maybe String
        existsAlready [] = Nothing  
        existsAlready (CStmtStr labelName str:ls) =
            if str == s then Just labelName
            else existsAlready ls

        findFreeLabel :: [ConstStmt] -> Int
        findFreeLabel [] = 0
        findFreeLabel (CStmtStr ('s':'t':'r':'_':num) _:ls) =
            let n = 1 + read num :: Int
            in max n (findFreeLabel ls)
        findFreeLabel (_:ls) = findFreeLabel ls

    in case existsAlready consts of 
        (Just label) -> (PrintStmt nl (Variabl label), consts) 
        Nothing -> 
            let label = "str_" ++ show (findFreeLabel consts)
            in  (PrintStmt nl (Variabl label), CStmtStr label s:consts))

printLiteralHelper ls = return ls


printLiteralProcessor :: [Stmt] -> State [ConstStmt] [Stmt]
printLiteralProcessor [] = return []
printLiteralProcessor (st:stmts) = do
    stNew <- printLiteralHelper st
    stmtsFinal <- printLiteralProcessor stmts
    return (stNew:stmtsFinal)


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
                (finalStmts, finalConsts) = runState (printLiteralProcessor stmts) postConsts
                    --printLiteralProcessor postConsts stmts

            in  (finalConsts, CFunc name finalStmts args:postFuncs) 

        (preConsts, preFuncs, preAst) = parser (s ++ "\n")
        (ast, pre1consts) = runState (printLiteralProcessor preAst) preConsts
            --printLiteralProcessor preConsts preAst
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
    let (asm, table) = runState (translator "" ast) varTable

    print asmData
    
    let out = generateText (asm, asmData)

    --putStrLn out
    writeFile outFileName out

