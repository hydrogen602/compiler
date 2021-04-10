{
module Grammar (parser, Stmt(..), G_Variable(..), G_Type(..)) where

import Data.Char
import Lexer
import Token
import Zahlen2 ( Constraint(..) )
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      let             { Let }
      in              { In }
      int             { Integer $$ }
      var             { Var $$ }
      where           { Where }
      inf             { Infinity }
      U               { Union }
      print           { Print }

      '\n'            { NewLine }
      ')'             { RParens }
      '='             { Equals }
      '('             { LParens }
      '['             { LSqB }
      ']'             { RSqB }
      ','             { Comma }
      '-'             { Minus }
%%

Block   : Stmt '\n' Block                          { ($1):($3) }
        | Stmt '\n'                                { [$1] }

Stmt    : let var '=' int where var in var Cnstrnt { letStmtHelper $2 $4 $6 $8 $9 }
        | var '=' var                              { AssignStmt (Variabl $1) (Variabl $3) }
        | print var                                { PrintStmt (Variabl $2) }

Cnstrnt : {- empty -}                              { [] }
        | Cnstrnt2                                 { $1 }
        | '(' '-' inf ',' int ']'                  { [MaxOrEq $5] }
        | '(' '-' inf ',' int ']' U Cnstrnt2       { (MaxOrEq $5):($8) }

Cnstrnt2 : '[' int ',' int ']'                      { [Range $2 $4] }
         | '[' int ',' int ']' U Cnstrnt2           { (Range $2 $4):$7 }
         | '[' int ',' inf ')'                      { [MinOrEq $2] }


{
parseError :: [Token] -> a
parseError tok = error $ "Parse error " ++ show tok

letStmtHelper :: String -> Integer -> String -> String -> [Constraint] -> Stmt
letStmtHelper name val name2 type_ ls =
      if name == name2 then LetStmt (Variabl name) val (Typ type_) ls
      else error "G_Variable names do not match"

data Stmt = 
        LetStmt G_Variable Integer G_Type [Constraint]
      | AssignStmt G_Variable G_Variable
      | PrintStmt G_Variable
      deriving Show

data G_Variable = Variabl String deriving Show
data G_Type = Typ String deriving Show

parser :: String -> [Stmt]
parser s = calc $ lexThis s

--main = do
--      s <- getContents
--      putStrLn $ show $ calc (lexThis s)
--}