module Token where

import           Extras.Misc (FixedAnnotated (..))


-- The token type:
data Token =
  NewLine     |
  Let         |
  If          |
  Else        |
  While       |
  Def         |
  Equals      |
  LParens     |
  RParens     |
  LCurly      |
  RCurly      |
  LSqB        |
  RSqB        |
  Comma       |
  Colon       |
  Return      |
  Print Bool  |
  Const       |
  RightArrow  |
  Sym Char    |
  Var String  |
  Integer Int |
  Str String
  deriving (Eq,Show)
