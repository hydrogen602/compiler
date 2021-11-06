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
      return          { Return }

      '\n'            { NewLine }
      ')'             { RParens }
      '='             { Equals }
      '('             { LParens }
      '['             { LSqB }
      ']'             { RSqB }
      '{'             { LCurly }
      '}'             { RCurly }
      ','             { Comma }
      ':'             { Colon }
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
        | Block                                    { ([], [], ($1)) }

CStmt   : const var '=' str                        { Left $ CStmtStr $2 $4 }
        | def var '(' Params ')' '{' Block Return '}'       { Right $ CFunc $2 (($7)++($8)) $4 }

Return  : return var '\n'                          { [ReturnStmt $2] }
        | {- Empty -}                              { [] }

Params  : var Params2                            { ($1):($2) }
        | {- Empty -}                              { [] }

Params2 : ',' var Params2                        { ($2):($3) }
        | {- Empty -}                              { [] }

-- Typed   : var ':' var                              { TypedParam ($1) ($3) }

Args    :: { [Expr] }
        : Expr Args2                               { ($1):($2) }
        | {- Empty -}                              { [] }

Args2   :: { [Expr] }
        : ',' Expr Args2                           { ($2):($3) }
        | {- Empty -}                              { [] }


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
        | var '(' Args ')'                         { FuncCall $1 $3 }

ElseP   : else '{' Block '}'                       { $3 }
        | {- Empty -}                              { [] }

Expr    : Expr '+' Expr                            { Expr '+' $1 $3 }
        | Expr '<' Expr                            { Expr '<' $1 $3 }
        | Value                                    { $1 }
        | var '(' Args ')'                         { FuncExpr $1 $3 }

Value   : var                                      { Variabl $1 }
        | int                                      { Immediate $1 }

{
parseError :: [Token] -> a
parseError tok = error $ "Parse error " ++ show tok

parser :: String -> AST
parser s = let x = lexThis s in calc (trace (show x) x)

startHelper :: Either ConstStmt Function -> AST -> AST
startHelper (Left c) (consts, funcs, stmts) = (c:consts, funcs, stmts)
startHelper (Right f) (consts, funcs, stmts) = (consts, f:funcs, stmts)
}