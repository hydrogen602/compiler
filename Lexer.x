{
module Lexer (lexThis) where

import Data.Char
import Token
}

%wrapper "basic"

$digit = 0-9		  	-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$notv = [^$alpha $digit \_]
$aw = [$white \n]

tokens :-

  \n$white*             { \s -> NewLine }
  $white+				        ;
  "--".*\n*				      ;
  let  	                { \s -> Let }
  const                 { \s -> Const }
  if                    { \s -> If }
  \n*else$aw*           { \s -> Else }
  while                 { \s -> While }
  def                   { \s -> Def }
  println               { \s -> Print True }
  print                 { \s -> Print False }
  "-"?$digit+				    { \s -> Integer (read s) }
  true                  { \s -> Integer 1 }
  false                 { \s -> Integer 0 }
  return                { \s -> Return }
  \"[^\"]*\"            { strHelper }
  \(                    { \s -> LParens }
  \)                    { \s -> RParens }
  \[                    { \s -> LSqB }
  \]                    { \s -> RSqB }
  \{$aw*                { \s -> LCurly }
  \}                    { \s -> RCurly }
  \=                    { \s -> Equals }
  \,                    { \s -> Comma }
  \:                    { \s -> Colon }
  \-\>                  { \s -> RightArrow }
  [\+\-\*\/\<\>]        { \s -> Sym (head s) }
  $alpha [$alpha $digit \_]*		{ \s -> Var s }

{

-- Each action has type :: String -> Token

-- let n = 0 where n \in Zahlen with (-\inf, 5]U(7,9)

strHelper :: String -> Token
strHelper ('\"':sTmp) = 
  let s = init sTmp
--      helper "" = ""
--      helper ('\\':'n':ls) = '\n':(helper ls)
--      helper ('\\':'t':ls) = '\t':(helper ls)
--      helper ('\\':'r':ls) = '\r':(helper ls)
--      helper ('\\':'\\':ls) = '\\':(helper ls)
--      helper ('\\':c:ls) = error $ "Found escape seq: \\" ++ [c] ++ " but can't understand that"
--      helper (s:ls) = s:(helper ls)

  in Str s --(helper s)

-- main = do
--   s <- getContents
--   print (alexScanTokens s)
--

lexThis s = alexScanTokens s

}
