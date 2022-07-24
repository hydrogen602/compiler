{
module Grammar (parser, Stmt(..), Expr(..)) where

import Data.Char
import Lexer
import Token
import Debug.Trace
import Util.AST
import Util.Types
import Util.Literals
import qualified Data.Map.Strict as Map
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

Start   : '\n' Start                               { $2 }
        | CStmt '\n' Start                         { startHelper ($1) ($3) }
        | Block Return                             { fromStmts ($1 ++ $2) }

CStmt   : const var '=' str                            { Left (ConstName ($2), ConstValueStr ($4)) }
        | def var '(' Params ')' '{' Block Return '}'  { Right (ASTFunction (FunctionName ($2)) ($4) (($7)++($8))) }

Return  : return Expr '\n'                          { [ReturnStmt ($2)] }
        | {- Empty -}                              { [] }

Params  : var Params2                              { (LocalVariable $1):($2) }
        | {- Empty -}                              { [] }

Params2 : ',' var Params2                          { (LocalVariable $2):($3) }
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

Stmt    : let var '=' Expr                         { LetStmt (LocalVariable $2) $4 }
        | var '=' Expr                             { AssignStmt (LocalVariable $1) $3 }
        | println str                              { PrintLiteralStmt UseNewLine $2 }
        | print str                                { PrintLiteralStmt NoUseNewLine $2 }
        | println Expr                             { PrintStmt UseNewLine $2 }
        | print Expr                               { PrintStmt NoUseNewLine $2 }
        | if Expr '{' Block '}' ElseP              { IfStmt $2 $4 $6 }
        | while Expr '{' Block '}'                 { WhileStmt $2 $4 }
        | var '(' Args ')'                         { FuncCall (FunctionName $1) $3 }

ElseP   : else '{' Block '}'                       { $3 }
        | {- Empty -}                              { [] }

Expr    : Expr '+' Expr                            { Expr ADD $1 $3 }
        | Expr '<' Expr                            { Expr LESS_THAN $1 $3 }
        | Value                                    { $1 }
        | var '(' Args ')'                         { FuncExpr (FunctionName $1) $3 }
        | '(' Expr ')'                             { $2 }

Value   : var                                      { Variabl (LocalVariable $1) }
        | int                                      { Immediate $1 }

{
parseError :: [Token] -> a
parseError tok = error $ "Parse error " ++ show tok

parser :: String -> AST
parser = calc . lexThis
}