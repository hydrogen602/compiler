module Token where


-- The token type:
data Token =
  NewLine     |
  Let         |
  Mut         |
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
  Dot         |
  Colon       |
  Return      |
  Const       |
  RightArrow  |
  Sym BiOpSym |
  Minus       |
  Var String  |
  Integer Int |
  Str String
  deriving (Show, Eq, Ord)


newtype BiOpSym =
    InnerOpSymBinary String
  deriving (Show, Eq, Ord)

-- "-" is special cause it can either be minus (binary) or negate (unary)
_allowedBinary :: [String]
_allowedBinary = ["<", ">", "+", "==", "<=", ">=", "!=", "/", "%", "*"]

fromCharToOp :: String -> BiOpSym
fromCharToOp c
  | c `elem` _allowedBinary = InnerOpSymBinary c
  | otherwise = error $ "InternalError: Unrecognized op: " ++ show c


