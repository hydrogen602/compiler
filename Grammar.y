{
module Grammar (parser, Stmt(..), Expr(..)) where

import Data.Char
import Lexer
import Token
import AST
import Debug.Trace
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      let             { Let }
      const           { Const }
      if              { If }
      else            { Else }
      while           { While }
      int             { Integer $$ }
      var             { Var $$ }
      def             { Def }
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
      '-'             { Sym '-' }
      '+'             { Sym '+' }
      '<'             { Sym '<' }

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%

Start   : CStmt '\n' Start                         { startHelper ($1) ($3) }
        | Block                                    { ([], ($1)) }

CStmt   : const var '=' str                        { CStmtStr $2 $4 }
        | def var '(' ')' '{' Block '}'            { CFunc $2 $6 }

Block   : Stmt '\n' Block                          { ($1):($3) }
        | {- Empty -}                              { [] }

Stmt    : let var '=' Expr                         { LetStmt $2 $4 }
        | var '=' Expr                             { AssignStmt $1 $3 }
        | println str                              { PrintLiteralStmt True $2 }
        | print str                                { PrintLiteralStmt False $2 }
        | println Expr                             { PrintStmt True $2 }
        | print Expr                               { PrintStmt False $2 }
        | if Expr '{' Block '}' ElseP              { IfStmt $2 $4 $6 }
        | while Expr '{' Block '}'                 { WhileStmt $2 $4 }

ElseP   : else '{' Block '}'                       { $3 }
        | {- Empty -}                              { [] }

Expr    : Expr '+' Expr                            { Expr '+' $1 $3 }
        | Expr '<' Expr                            { Expr '<' $1 $3 }
        | Value                                    { $1 }

Value   : var                                      { Variabl $1 }
        | int                                      { Immediate $1 }

{
parseError :: [Token] -> a
parseError tok = error $ "Parse error " ++ show tok

parser :: String -> AST
parser s = let x = lexThis s in calc (trace (show x) x)

startHelper :: ConstStmt -> AST -> AST
startHelper c (consts, stmts) = (c:consts, stmts)
}