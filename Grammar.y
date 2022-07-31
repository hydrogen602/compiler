{
module Grammar (parser, Stmt(..), Expr(..)) where

import Data.Char
import qualified Data.Map.Strict as Map
import Debug.Trace

import Lexer
import Token
import Util.AST
import Util.Types
import Util.Literals
import Types.Addon
import Types.Core


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
      right_arrow     { RightArrow }

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

CStmt   : const var '=' str                        { Left (ConstName ($2), ConstValueStr ($4)) }
        | Func                                     { Right $1 }

Func    : def var '(' MParams ')' right_arrow var '{' Block Return '}'  { ASTFunction (FunctionName ($2)) ($4) (typeHelper $7 ()) (($9)++($10)) }

Return  : return Expr '\n'                         { [ReturnStmt ($2)] }
        | {- Empty -}                              { [] }

MParams : Params                                   { $1 }
        | {- Empty -}                              { [] }

Params  : var ':' var Params2                      { (typeHelper $3 (LocalVariable $1)):($4) }

Params2 : ',' Params                               { $2 }
        | {- Empty -}                              { [] }

-- Typed   : var ':' var                              { TypedParam ($1) ($3) }

Args    :: { [MaybeTyped (Expr MaybeTyped)] }
        : Expr Args2                               { ($1):($2) }
        | {- Empty -}                              { [] }

Args2   :: { [MaybeTyped (Expr MaybeTyped)] }
        : ',' Expr Args2                           { ($2):($3) }
        | {- Empty -}                              { [] }


Block   : Stmt '\n' Block                          { ($1):($3) }
        | {- Empty -}                              { [] }

Stmt    :: { Stmt MaybeTyped }
        : let var '=' Expr                         { LetStmt (LocalVariable $2) $4 }
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

Expr    :: { MaybeTyped (Expr MaybeTyped) }
        : Expr '+' Expr                            { noType (Expr ADD $1 $3) }
        | Expr '<' Expr                            { noType (Expr LESS_THAN $1 $3) }
        | Value                                    { noType ($1) }
        | var '(' Args ')'                         { noType (FuncExpr (FunctionName $1) $3) }
        | '(' Expr ')'                             { ($2) }

Value   : var                                      { Variabl (LocalVariable $1) }
        | int                                      { Immediate $1 }

{
typeHelper :: String -> a -> Typed a
typeHelper s = Typed $ TypeName s

noType :: a -> MaybeTyped a
noType = MaybeTyped Nothing

parseError :: [Token] -> a
parseError tok = error $ "Parse error " ++ show tok

parser :: String -> AST MaybeTyped
parser = calc . lexThis
}