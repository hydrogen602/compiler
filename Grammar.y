{
module Grammar (parser, Stmt(..), G_Value(..)) where

import Data.Char
import Lexer
import Token
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
      '{'             { LCurly }
      '}'             { RCurly }
      ','             { Comma }
      '-'             { Minus }
%%

Block   : Stmt '\n' Block                          { ($1):($3) }
        | Stmt '\n'                                { [$1] }

Stmt    : let var '=' int                          { LetStmt $2 (Immediate $4) }
        | var '=' var                              { AssignStmt $1 (Variabl $3) }
        | print var                                { PrintStmt (Variabl $2) }

{
parseError :: [Token] -> a
parseError tok = error $ "Parse error " ++ show tok

data Stmt = 
        LetStmt String G_Value
      | AssignStmt String G_Value
      | PrintStmt G_Value
      deriving Show

data G_Value = Variabl String | Immediate Int deriving Show

parser :: String -> [Stmt]
parser s = calc $ lexThis s
}