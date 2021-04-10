
import Data.Char
import Data.List
import Grammar
import Asm
import AST

type VariableTracker = ([(String, String)], [String]) -- variables (registers) & string data labels


getRegister :: String -> VariableTracker -> String 
getRegister name (varTable, _) =
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



translate :: Stmt -> VariableTracker -> [Line]
translate (LetStmt name val) varTable = 
    let register = getRegister name varTable
    in case val of
            (Variabl v) -> asmSetToRegister register (getRegister v varTable)
            (Immediate n) -> asmSetToImmediate register n

translate (AssignStmt name val) varTable = 
    let register1 = getRegister name varTable
    in case val of
            (Variabl v) -> asmSetToRegister register1 (getRegister v varTable)
            (Immediate n) -> asmSetToImmediate register1 n

translate (PrintStmt (Variabl name)) (varTable, labels) = 
    case lookup name varTable of
        (Just register) -> asmPrintReg register
        Nothing -> 
            if name `elem` labels then asmPrintConstStr name 
            else error $ "Cannot find variable: " ++ name
    -- let register = getRegister name varTable
    -- in asmPrintReg register

translate (PrintStmt (Immediate n)) varTable = asmPrintInt n

--translate (ConstStmt name value) varTable = undefined 

translateData :: [ConstStmt] -> ([AsmData], [String])
translateData [] = ([], [])
translateData ((CStmtStr name value):ls)
    | name == "main" = error "Cannot use label main"
    | name `elem` labels = error $ "Label already used: " ++ name
    | otherwise = ((AsmString name value):aData, name:labels)
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
    let asm = concatMap (`translate` (registerTable, dataLabels)) ast

    let out = generateText (asm, asmData)

    putStrLn out
    writeFile "out.s" out

