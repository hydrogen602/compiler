{
module Lexer (lexThis) where

import Token
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  \n$white*             { \s -> NewLine }
  $white+				        ;
  "--".*				        ;
  let					          { \s -> Let }
  in					          { \s -> In }
  print                 { \s -> Print }
  inf                   { \s -> Infinity }
  where                 { \s -> Where }
  U                     { \s -> Union }
  $digit+				        { \s -> Integer (read s) }
  \(                    { \s -> LParens }
  \)                    { \s -> RParens }
  \[                    { \s -> LSqB }
  \]                    { \s -> RSqB }
  \{                    { \s -> LCurly }
  \}                    { \s -> RCurly }
  \=                    { \s -> Equals }
  \,                    { \s -> Comma }
  \-                    { \s -> Minus }
  [\+\*\/]			{ \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*		{ \s -> Var s }

{

-- Each action has type :: String -> Token

-- let n = 0 where n \in Zahlen with (-\inf, 5]U(7,9)


-- main = do
--   s <- getContents
--   print (alexScanTokens s)
--

lexThis s = alexScanTokens s

}
