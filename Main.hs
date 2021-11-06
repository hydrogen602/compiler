
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
import Translator


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

    -- read input
    s <- case argumentExtract "-i" args of
        (Just inFile) -> readFile inFile
        Nothing  -> getContents 

    let -- config output
        outFileName = case argumentExtract "-o" args of
            (Just outFile) -> outFile
            Nothing -> "out.s"

        funcHelper :: [ConstStmt] -> [Function] -> ([ConstStmt], [Function])
        funcHelper consts [] = (consts, [])
        funcHelper consts ((CFunc name stmts args):funcs) = 
            let (postConsts, postFuncs) = funcHelper consts funcs
                (finalStmts, finalConsts) = runState (printLiteralProcessor stmts) postConsts
                    --printLiteralProcessor postConsts stmts

            in  (finalConsts, CFunc name finalStmts args:postFuncs) 

        -- parse
        (preConsts, preFuncs, preAst) = parser (s ++ "\n")
        -- process constants, literals
        (ast, pre1consts) = runState (printLiteralProcessor preAst) preConsts
            --printLiteralProcessor preConsts preAst
        -- process constants, literals in functions
        (consts, funcs) = funcHelper pre1consts preFuncs

    putStr "consts = "
    print consts
    putStr "ast = "
    print ast

    --let state = evalAST ast (LocalSt [])

    --putStrLn (show state)

    -- process consts
    let (preAsmData, preLabels) = translateData "" consts []
    putStr "preLabels = "
    print preLabels
    let (asmFuncs, labels) = translateFunc "" funcs preLabels 

        asmData = asmFuncs ++ preAsmData

        varTable = newVarTracker labels

    putStrLn "==================================="
    print varTable
    putStrLn "==================================="
    -- translate ast into asm
    let (asm, table) = runState (translator "" ast) varTable

    print asmData
    
    let out = generateText (asm, asmData)

    --putStrLn out
    writeFile outFileName out

