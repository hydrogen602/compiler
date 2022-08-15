{
module Lexer (lexThis, WithPosition(..), getLineColPair) where

import Data.Char
import Token
}

%wrapper "posn"

$digit = 0-9		  	-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$notv = [^$alpha $digit \_]
$aw = [$white \n]

tokens :-

  \n$white*             { helper' NewLine }
  $white+				        ;
  "--".*\n*				      ;
  let  	                { helper' Let }
  mut                   { helper' Mut }
  const                 { helper' Const }
  if                    { helper' If }
  $aw*else$aw*           { helper' Else }
  while                 { helper' While }
  def                   { helper' Def }
  $digit+				        { helper  (Integer . read) }
  true                  { helper' (Integer 1) }
  false                 { helper' (Integer 0) }
  return                { helper' Return }
  \"[^\"]*\"            { helper  strHelper }
  \(                    { helper' LParens }
  \)                    { helper' RParens }
  \[                    { helper' LSqB }
  \]                    { helper' RSqB }
  \{$aw*                { helper' LCurly }
  \}                    { helper' RCurly }
  \=                    { helper' Equals }
  \,                    { helper' Comma }
  \:                    { helper' Colon }
  \-\>                  { helper' RightArrow }
  \-                    { helper' Minus }
  [\+\*\/\<\>\%]        { helper  (Sym . fromCharToOp) }
  \>\=|\<\=|\=\=|\!\=   { helper  (Sym . fromCharToOp) }
  $alpha [$alpha $digit \_]*		{ helper Var }

{

data WithPosition = WithPosition {
  pos   :: AlexPosn,
  token :: Token
  } deriving (Show, Eq)

getLineColPair :: AlexPosn -> (Int, Int)
getLineColPair (AlexPn _ line col) = (line,col)

helper :: (String -> Token) -> AlexPosn -> String -> WithPosition
helper convert pos = WithPosition pos . convert

helper' :: Token -> AlexPosn -> String -> WithPosition
helper' tok pos _ = WithPosition pos tok

-- Each action has type :: String -> Token

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
