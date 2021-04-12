
import Data.Char
import Data.List
import Grammar
import Asm
import AST
import Util
import Data

type VariableTracker = ([(String, String)], [String], Int) -- variables (registers) & string data labels & if label ids

type TmpReg = String

getRegister :: String -> VariableTracker -> String 
getRegister name (varTable, _, _) =
    case lookup name varTable of
        (Just s) -> s
        Nothing -> error $ "Cannot find variable: " ++ name



registerAssignHelper :: [Stmt] -> [String] -> [String] -> [(String, String)]
registerAssignHelper [] _ reg = []
registerAssignHelper ((LetStmt name val):ls) _ [] = 
    error $ "Ran out of registers to assign with var: " ++ name
registerAssignHelper ((LetStmt name val):ls) usedLabels (r:reg) = 
    let others = registerAssignHelper ls usedLabels reg
        conflict = any (\(n, _) -> n == name) others || name `elem` usedLabels

    in  if conflict then error $ "Variable name \"" ++ name ++ "\" is declared more than once"
        else (name, r):others
registerAssignHelper (_:ls) usedLabels reg = registerAssignHelper ls usedLabels reg

registerAssign :: [Stmt] -> [String] -> [(String, String)]
registerAssign stmts usedLabels = registerAssignHelper stmts usedLabels ["$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"]



expressionEvalHelper :: [TmpReg] -> Expr -> VariableTracker -> ([Line], String) -- returns lines and register where result is
expressionEvalHelper (register1:_) (Variabl s) varTable = 
    let register2 = getRegister s varTable
        code = asmSetToRegister register1 register2
    in (code, register1)

expressionEvalHelper (register:_) (Immediate v) varTable = 
    let code = asmSetToImmediate register v
    in (code, register)

expressionEvalHelper tmpRegLs (ExprPlus (Variabl s) e) varTable =
    let register = getRegister s varTable
        (code, reg) = expressionEvalHelper tmpRegLs e varTable
        codeAdd = asmAddRegisters reg register reg
    in (code ++ codeAdd, reg)

expressionEvalHelper tmpRegLs (ExprPlus (Immediate v) e) varTable =
    let (code, reg) = expressionEvalHelper tmpRegLs e varTable
        codeAdd = asmAddImmediate reg reg v
    in  (code ++ codeAdd, reg)

expressionEvalHelper (tmpReg:tmpRegLs) (ExprPlus e1 e2) varTable = 
    let (code1, reg1) = expressionEvalHelper (tmpReg:tmpRegLs) e1 varTable
        (code2, reg2) = expressionEvalHelper tmpRegLs e2 varTable
        set2 = asmAddRegisters reg1 reg1 reg2
    in (concat [code1, code2, set2], reg1)



expressionEval :: Expr -> VariableTracker -> ([Line], String)
expressionEval = expressionEvalHelper tmpRegs

expressionSetReg :: String -> Expr -> VariableTracker -> [Line]
expressionSetReg s expr varTable = fst $ expressionEvalHelper (s:tmpRegs) expr varTable


translate :: Stmt -> VariableTracker -> [Line]
translate (LetStmt name val) varTable = 
    let register = getRegister name varTable
    in case val of
            (Variabl v) -> asmSetToRegister register (getRegister v varTable)
            (Immediate n) -> asmSetToImmediate register n
            expr@(ExprPlus e1 e2) -> 
                let --(code, reg) = expressionEval expr varTable
                    --moveCode = asmSetToRegister register reg
                    code = expressionSetReg register expr varTable
                in code -- ++ moveCode

translate (AssignStmt name val) varTable = 
    let register = getRegister name varTable
    in case val of
            (Variabl v) -> asmSetToRegister register (getRegister v varTable)
            (Immediate n) -> asmSetToImmediate register n
            expr@(ExprPlus e1 e2) -> 
                let --(code, reg) = expressionEval expr varTable
                    --moveCode = asmSetToRegister register1 reg
                    code = expressionSetReg register expr varTable
                in code -- ++ moveCode

translate (PrintStmt withNL (Variabl name)) (varTable, labels, _) = 
    (case lookup name varTable of
        (Just register) -> asmPrintReg register
        Nothing -> 
            if name `elem` labels then asmPrintConstStr name 
            else error $ "Cannot find variable: " ++ name

    ) ++ if withNL then printNewLineCall else [] 


translate (PrintStmt withNL (Immediate n)) varTable = asmPrintInt n ++ if withNL then printNewLineCall else []

translate (IfStmt condition block) (vars, stringLabels, ifLabelNum) = 
    let varTable = (vars, stringLabels, ifLabelNum + 1)

        innerBlock = concatMap (`translate` varTable) block
        ifLabel = "if_end_" ++ show ifLabelNum

        preCode = [
            EmptyLine,
            Instruction "beq" [getRegister condition varTable, "$0", ifLabel]
            ]
        postCode = [
            EmptyLine,
            Label ifLabel
            ]
    in preCode ++ innerBlock ++ postCode

--translate (ConstStmt name value) varTable = undefined 

translateData :: [ConstStmt] -> ([AsmData], [String])
translateData [] = ([], [])
translateData ((CStmtStr name value):ls)
    | name == "main" = error "Cannot use label main"
    | "if_end_" `isPrefixOf` name = error "Label cannot start with 'if_end_'"
    | name `elem` labels = error $ "Label already used: " ++ name
    | otherwise = (AsmString name value:aData, name:labels)
    where (aData, labels) = translateData ls


main :: IO ()
main = do
    s <- getContents 

    let (consts, ast) = parser (s ++ "\n")

    print consts
    print ast

    --let state = evalAST ast (LocalSt [])

    --putStrLn (show state)

    let (asmData, dataLabels) = translateData consts

    let registerTable = registerAssign ast dataLabels
    let asm = concatMap (`translate` (registerTable, dataLabels, 0)) ast

    let out = generateText (asm, asmData)

    --putStrLn out
    writeFile "out.s" out

