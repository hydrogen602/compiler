{
module Grammar (parser, Stmt(..), Expr(..)) where

import Data.Char
import Lexer
import Token
import AST
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      let             { Let }
      const           { Const }
      in              { In }
      if              { If }
      else            { Else }
      int             { Integer $$ }
      var             { Var $$ }
      where           { Where }
      inf             { Infinity }
      U               { Union }
      println         { Print True }
      print           { Print False }
      str             { Str $$ }

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
      '+'             { Plus }
%%

Start   : CStmt '\n' Start                         { startHelper ($1) ($3) }
        | Block                                    { ([], ($1)) }

CStmt   : const var '=' str                        { CStmtStr $2 $4 }

Block   : Stmt '\n' Block                          { ($1):($3) }
        | Stmt '\n'                                { [$1] }

Stmt    : let var '=' Expr                         { LetStmt $2 $4 }
        | var '=' Expr                             { AssignStmt $1 $3 }
        | println Expr                             { PrintStmt True $2 }
        | print Expr                               { PrintStmt False $2 }
        | if Expr '{' Block '}' ElseP              { IfStmt $2 $4 $6 }

ElseP   : else '{' Block '}'                       { $3 }
        | {- Empty -}                              { [] }

Expr    : Value '+' Expr                           { ExprPlus $1 $3 }
        | Value                                    { $1 }

Value   : var                                      { Variabl $1 }
        | int                                      { Immediate $1 }

{
parseError :: [Token] -> a
parseError tok = error $ "Parse error " ++ show tok

parser :: String -> AST
parser s = calc $ lexThis s

startHelper :: ConstStmt -> AST -> AST
startHelper c (consts, stmts) = (c:consts, stmts)
}