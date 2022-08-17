{
module Grammar (parser, Stmt(..), Expr(..)) where

import Data.List (intercalate)

import Lexer
import Token
import Core.AST
import Core.Types
import Core.Literals
import Types.Addon
import Types.Core
import Extras.Position (Pos(Pos))

}

%name calc
%tokentype { WithPosition }
%error { parseError }
%errorhandlertype explist

%token 
      let             { WithPosition $$ Let }
      mut             { WithPosition $$ Mut }
      const           { WithPosition _ Const }
      if              { WithPosition $$ If }
      else            { WithPosition _ Else }
      while           { WithPosition _ While }
      int             { WithPosition _ (Integer $$) }
      var             { WithPosition _ (Var $$) }
      def             { WithPosition $$ Def }
      str             { WithPosition _ (Str $$) }
      return          { WithPosition _ Return }
      right_arrow     { WithPosition _ RightArrow }

      '\n'            { WithPosition _ NewLine }
      ')'             { WithPosition _ RParens }
      '='             { WithPosition $$ Equals }
      '('             { WithPosition _ LParens }
      '['             { WithPosition _ LSqB }
      ']'             { WithPosition _ RSqB }
      '{'             { WithPosition _ LCurly }
      '}'             { WithPosition _ RCurly }
      ','             { WithPosition _ Comma }
      '.'             { WithPosition _ Dot }
      ':'             { WithPosition _ Colon }

      '-'             { WithPosition _ Minus }

      '+'             { WithPosition _ (Sym (InnerOpSymBinary "+")) }
      '<'             { WithPosition _ (Sym (InnerOpSymBinary "<")) }
      '>'             { WithPosition _ (Sym (InnerOpSymBinary ">")) }
      '*'             { WithPosition _ (Sym (InnerOpSymBinary "*")) }
      '/'             { WithPosition _ (Sym (InnerOpSymBinary "/")) }
      '%'             { WithPosition _ (Sym (InnerOpSymBinary "%")) }
      gte             { WithPosition _ (Sym (InnerOpSymBinary ">=")) }
      lte             { WithPosition _ (Sym (InnerOpSymBinary "<=")) }
      eq              { WithPosition _ (Sym (InnerOpSymBinary "==")) }
      neq             { WithPosition _ (Sym (InnerOpSymBinary "!=")) }


%nonassoc '>' '<' gte lte eq neq
%left '+' '-'
%left '*' '/' '%'
%left NEG
%left '.'


%%

Start   : '\n' Start                               { $2 }
        | CStmt '\n' Start                         { startHelper ($1) ($3) }
        | Block Return                             { fromStmts ($1 ++ $2) }

CStmt   : const var '=' str                        { Left (ConstName ($2), ConstValueStr ($4)) }
        | Func                                     { Right $1 }

Func    : def var '(' MParams ')' right_arrow var '{' Block Return '}'  { ASTFunction (toPos $1) (FunctionName ($2)) ($4) (typeHelper $7 ()) (($9)++($10)) }

Return  : return Expr '\n'                         { [ReturnStmt ($2)] }
        | {- Empty -}                              { [] }

MParams : Params                                   { $1 }
        | {- Empty -}                              { [] }

Params  : var ':' var Params2                      { (typeHelper $3 (LocalVariable $1)):($4) }

Params2 : ',' Params                               { $2 }
        | {- Empty -}                              { [] }


Args    :: { [MaybeTyped (Expr MaybeTyped)] }
        : Expr Args2                               { ($1):($2) }
        | {- Empty -}                              { [] }

Args2   :: { [MaybeTyped (Expr MaybeTyped)] }
        : ',' Expr Args2                           { ($2):($3) }
        | {- Empty -}                              { [] }


Block   : Stmt '\n' Block                          { ($1):($3) }
        | Stmt                                     { [$1] }
        | {- Empty -}                              { [] }

Stmt    :: { Stmt MaybeTyped }
        : let mut var '=' Expr                     { LetMutStmt (toPos $1) (LocalVariable $3) $5 }
        | let var '=' Expr                         { LetStmt (toPos $1) (LocalVariable $2) $4 }
        | var '=' Expr                             { AssignStmt (toPos $2) (LocalVariable $1) $3 }
        | while Expr '{' Block '}'                 { WhileStmt $2 $4 }
        | Expr                                     { ExprStmt $1 }

ElseP   : else '{' Block '}'                       { $3 }
        | {- Empty -}                              { [] }

Expr    :: { MaybeTyped (Expr MaybeTyped) }
        : if Expr '{' Block '}' ElseP              { noType (IfExpr (toPos $1) $2 $4 $6) }
        | Expr '+' Expr                            { noType (Expr ADD $1 $3) }
        | Expr '-' Expr                            { noType (Expr SUB $1 $3) }
        | Expr '<' Expr                            { noType (Expr LESS_THAN $1 $3) }
        | Expr '>' Expr                            { noType (Expr GREATER_THAN $1 $3) }
        | Expr lte Expr                            { noType (Expr LESS_THAN_EQUAL $1 $3) }
        | Expr gte Expr                            { noType (Expr GREATER_THAN_EQUAL $1 $3) }
        | Expr eq Expr                             { noType (Expr EQUAL $1 $3) }
        | Expr neq Expr                            { noType (Expr NOT_EQUAL $1 $3) }
        | Expr '%' Expr                            { noType (Expr MOD $1 $3) }
        | Expr '*' Expr                            { noType (Expr PROD $1 $3) }
        | Expr '/' Expr                            { noType (Expr DIV $1 $3) }
        | '-' Expr %prec NEG                       { noType (Unary NEG $2) }
        | Value                                    { noType ($1) }
        | var '(' Args ')'                         { noType (FuncExpr (FunctionName $1) $3) }
        | Expr '.' var '(' Args ')'                { noType (DotFuncExpr (FunctionName $3) $1 $5) }
        | '(' Expr ')'                             { ($2) }

Value   : var                                      { Variabl (LocalVariable $1) }
        | int                                      { Immediate $1 }

{
typeHelper :: String -> a -> Typed a
typeHelper s = Typed $ TypeName s

toPos = uncurry Pos . getLineColPair

noType :: a -> MaybeTyped a
noType = MaybeTyped Nothing

parseError :: ([WithPosition], [String]) -> a
parseError (tok, possible) = error $ "Parse error\n" ++ show tok ++ "\n\nExpected one of: " ++ p
  where
    p = intercalate ", " possible

parser :: String -> AST MaybeTyped
parser = calc . lexThis
}