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
import Extras.Position (Pos(Pos))


}

%name calc
%tokentype { WithPosition }
%error { parseError }

%token 
      let             { WithPosition $$ Let }
      const           { WithPosition _ Const }
      if              { WithPosition _ If }
      else            { WithPosition _ Else }
      while           { WithPosition _ While }
      int             { WithPosition _ (Integer $$) }
      var             { WithPosition _ (Var $$) }
      def             { WithPosition _ Def }
      println         { WithPosition _ (Print True) }
      print           { WithPosition _ (Print False) }
      str             { WithPosition _ (Str $$) }
      return          { WithPosition _ Return }
      right_arrow     { WithPosition _ RightArrow }

      '\n'            { WithPosition _ NewLine }
      ')'             { WithPosition _ RParens }
      '='             { WithPosition _ Equals }
      '('             { WithPosition _ LParens }
      '['             { WithPosition _ LSqB }
      ']'             { WithPosition _ RSqB }
      '{'             { WithPosition _ LCurly }
      '}'             { WithPosition _ RCurly }
      ','             { WithPosition _ Comma }
      ':'             { WithPosition _ Colon }
      '-'             { WithPosition _ (Sym '-') }
      '+'             { WithPosition _ (Sym '+') }
      '<'             { WithPosition _ (Sym '<') }

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
        : let var '=' Expr                         { LetStmt (toPos $1) (LocalVariable $2) $4 }
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

toPos = uncurry Pos . getLineColPair

noType :: a -> MaybeTyped a
noType = MaybeTyped Nothing

parseError :: [WithPosition] -> a
parseError tok = error $ "Parse error " ++ show tok

parser :: String -> AST MaybeTyped
parser = calc . lexThis
}