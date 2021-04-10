
import Data.Char
import Data.List

import qualified Zahlen2 ( isSubSetOf )
import Zahlen2 (Constraint(..), z)

import Grammar

zahlen_isSubSetOf = Zahlen2.isSubSetOf

data Type =
    Zahl_t [Constraint]

instance Show Type where
    show (Zahl_t []) = "Zahl"
    show (Zahl_t ls) = 
        let helper [constr] = show constr
            helper (constr:ls) = show constr ++ "U" ++ helper ls
        in "Zahl " ++ helper ls

isSubSetOf :: Type -> Type -> Bool 
isSubSetOf (Zahl_t c1) (Zahl_t c2) = c1 `zahlen_isSubSetOf` c2
isSubSetOf _ _ = False 

data LocalSt =
    LocalSt [(String, Type)]
    deriving Show

varExists :: LocalSt -> String -> Bool 
varExists (LocalSt ls) s =
    let func [] = False
        func ((name, _):ls) = name == s || func ls
    in func ls

fetchType :: LocalSt -> String -> Type
fetchType (LocalSt ls) s =
    let func :: [(String, Type)] -> Type
        func [] = error $ "Variable name not found: " ++ s
        func ((name, type_):ls) = 
            if name == s then type_ else func ls
    in func ls

evalAST :: [Stmt] -> LocalSt -> LocalSt
evalAST ((LetStmt (Variabl s) n (Typ t) constr):ls) loc@(LocalSt vars) = 
    if varExists loc s then
        error $ "Variable name '" ++ s ++ "' exists already"
    else
        evalAST ls (LocalSt ((s, Zahl_t constr):vars))

evalAST ((AssignStmt (Variabl a) (Variabl b)):ls) loc@(LocalSt vars) =
    let typeA = fetchType loc a
        typeB = fetchType loc b
    in
        if typeB `isSubSetOf` typeA then 
            evalAST ls loc
        else
            error $ "\nTypeError: '" ++ show typeA ++ "' cannot be assigned to '" ++ show typeB ++ "'\n"

evalAST ((PrintStmt (Variabl a)):ls) loc =
    let _ = fetchType loc a
    in evalAST ls loc

evalAST [] loc = loc

registerAssignHelper :: [Stmt] -> [String] -> [(String, String)]
registerAssignHelper [] reg = []
registerAssignHelper ((LetStmt (Variabl name) _ _ _):ls) [] = 
    error $ "Ran out of registers to assign with var: " ++ name
registerAssignHelper ((LetStmt (Variabl name) _ _ _):ls) (r:reg) = 
    let others = registerAssignHelper ls reg
        conflict = any (\(n, _) -> n == name) others

    in  if conflict then error $ "Variable name \"" ++ name ++ "\" is declared more than once"
        else (name, r):others
registerAssignHelper (_:ls) reg = registerAssignHelper ls reg

registerAssign :: [Stmt] -> [(String, String)]
registerAssign stmts = registerAssignHelper stmts ["$s0", "$s1", "$s2", "$s3", "$s4", "$s5"]

translate :: Stmt -> [(String, String)] -> String
translate (LetStmt (Variabl name) n typ _) varTable = 
    let register = case lookup name varTable of
            (Just r) -> r
            Nothing  -> error $ "Cannot find variable: " ++ name
    in "\t\taddi \t" ++ register ++ ", $0, " ++ show n

translate (AssignStmt (Variabl n1) (Variabl n2)) varTable = 
    let register1 = case lookup n1 varTable of
            (Just r) -> r
            Nothing  -> error $ "Cannot find variable: " ++ n1
        register2 = case lookup n2 varTable of
            (Just r) -> r
            Nothing  -> error $ "Cannot find variable: " ++ n2
    in "\t\tadd \t" ++ register1 ++ ", $0, " ++ register2
translate (PrintStmt (Variabl name)) varTable = 
    let register = case lookup name varTable of
            (Just r) -> r
            Nothing  -> error $ "Cannot find variable: " ++ name
    in "\t\tmove \t$a0, " ++ register ++ "\n\t\tli  \t$v0, 1\n\t\tsyscall"

main :: IO ()
main = do
    s <- getContents 
    
    let ast = parser s

    putStrLn $ show ast

    let state = evalAST ast (LocalSt [])

    putStrLn (show state)

    let registerTable = registerAssign ast
    let asm = map (\ast -> translate ast registerTable) ast

    let header = "\t\t.text\n\t\t.globl  main\nmain:\n\n\t\taddi    $sp, $sp, -4\n\t\tsw      $ra, 0($sp)\n"
    let footer = "\n\t\tlw      $ra, 0($sp)\n\t\taddi    $sp, $sp, 4\n\t\tjr      $ra\n\t\t.end    main"

    let out = (header:asm) ++ [footer]
    
    putStrLn $ intercalate "\n" out
    writeFile "out.s" $ intercalate "\n" out

