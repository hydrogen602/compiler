
import Data.Char
import Data.List
import Grammar


getRegister :: String -> [(String, String)] -> String 
getRegister name varTable =
    case lookup name varTable of
        (Just s) -> s
        Nothing -> error $ "Cannot find variable: " ++ name


registerAssignHelper :: [Stmt] -> [String] -> [(String, String)]
registerAssignHelper [] reg = []
registerAssignHelper ((LetStmt name val):ls) [] = 
    error $ "Ran out of registers to assign with var: " ++ name
registerAssignHelper ((LetStmt name val):ls) (r:reg) = 
    let others = registerAssignHelper ls reg
        conflict = any (\(n, _) -> n == name) others

    in  if conflict then error $ "Variable name \"" ++ name ++ "\" is declared more than once"
        else (name, r):others
registerAssignHelper (_:ls) reg = registerAssignHelper ls reg

registerAssign :: [Stmt] -> [(String, String)]
registerAssign stmts = registerAssignHelper stmts ["$s0", "$s1", "$s2", "$s3", "$s4", "$s5"]



translate :: Stmt -> [(String, String)] -> String
translate (LetStmt name val) varTable = 
    let register = getRegister name varTable
        
        value = case val of
            (Variabl v) -> getRegister v varTable
            (Immediate n) -> show n
    in "\t\taddi \t" ++ register ++ ", $0, " ++ value

translate (AssignStmt name val) varTable = 
    let register1 = getRegister name varTable
    in case val of
            (Variabl v) ->
                let register2 = getRegister v varTable
                in "\t\tadd \t" ++ register1 ++ ", $0, " ++ register2
            (Immediate n2) ->
                "\t\taddi \t" ++ register1 ++ ", $0, " ++ show n2

translate (PrintStmt (Variabl name)) varTable = 
    let register = getRegister name varTable
    in "\t\tmove \t$a0, " ++ register ++ "\n\t\tli  \t$v0, 1\n\t\tsyscall"
translate (PrintStmt (Immediate n)) varTable =
    "\t\tli \t$a0, " ++ show n ++ "\n\t\tli  \t$v0, 1\n\t\tsyscall"

main :: IO ()
main = do
    s <- getContents 
    
    let ast = parser (s ++ "\n")

    putStrLn $ show ast

    --let state = evalAST ast (LocalSt [])

    --putStrLn (show state)

    let registerTable = registerAssign ast
    let asm = map (`translate` registerTable) ast

    let header = "\t\t.text\n\t\t.globl  main\nmain:\n\n\t\taddi    $sp, $sp, -4\n\t\tsw      $ra, 0($sp)\n"
    let footer = "\n\t\tlw      $ra, 0($sp)\n\t\taddi    $sp, $sp, 4\n\t\tjr      $ra\n\t\t.end    main"

    let out = (header:asm) ++ [footer]
    
    putStrLn $ intercalate "\n" out
    writeFile "out.s" $ intercalate "\n" out

