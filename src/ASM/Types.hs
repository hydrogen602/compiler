module ASM.Types where

import qualified Data.Map        as Map
import           Numeric.Natural (Natural)

import           Util.Classes    (Nameable)
import qualified Util.Classes    as Classes
import qualified Util.Literals   as Literals
import           Util.Types      (UseNewLine)
import qualified Util.Types


newtype Label = Label { getLabel :: String } deriving (Show, Eq, Ord)
instance Classes.Nameable Label where
  name = getLabel

data UnlimitedRegister =
    UnlimitedRegister { getNum :: Natural }
  | OneTypeSpecialRegister
  -- ^ a special register only for specific tasks in the ladder stages of the compiler
  deriving (Show, Eq, Ord)

instance Nameable UnlimitedRegister where
  name (UnlimitedRegister n)  = "U" ++ show n
  name OneTypeSpecialRegister = "UExtra"

newtype ASMLiteral = ASMLiteral { getLiteral :: Int } deriving (Show, Eq, Ord)

data BuiltinType =
    PrintInt UseNewLine
  | PrintString UseNewLine
  deriving (Show, Eq, Ord)

-- data SyscallType = PrintInt | PrintString deriving (Show, Eq, Ord)

-- instance Enum SyscallType where
--   toEnum 1 = PrintInt
--   toEnum 4 = PrintString
--   toEnum _ = error "Unsupported Syscall"

--   fromEnum PrintInt    = 1
--   fromEnum PrintString = 4

type ASMOp = Util.Types.Op
-- data ASMOp =
--     ADD
--   | LT
--   deriving (Show, Eq, Ord)
opToASM :: Util.Types.Op -> ASMOp
opToASM = id

data ASMUniaryOp =
  SET
  deriving (Show, Eq, Ord)

data Instruction =
    Branch UnlimitedRegister UnlimitedRegister Label
  | Binary ASMOp UnlimitedRegister UnlimitedRegister (Either UnlimitedRegister ASMLiteral)
  | Uniary ASMUniaryOp UnlimitedRegister (Either UnlimitedRegister ASMLiteral)
  | IfStmt UnlimitedRegister [ASMLine] Label [ASMLine] Label
  | WhileStmt Label UnlimitedRegister [ASMLine] Label
  | Jump Label
  | FuncCall {
    arguments :: [UnlimitedRegister],
    funcName  :: Label,
    result    :: Maybe UnlimitedRegister
  }
  | LoadLabel UnlimitedRegister Label
  | Builtin {
    arguments   :: [UnlimitedRegister],
    builtinType :: BuiltinType,
    result      :: Maybe UnlimitedRegister
  }
  --  | Syscall {
  --   arguments   :: [UnlimitedRegister],
  --   syscallType ::  SyscallType,
  --   result      :: Maybe UnlimitedRegister
  --   }
  deriving (Show, Eq, Ord)


type ASMLine = Either Instruction Label


data ASMData = ASMData {
  getNamedData   :: Map.Map Literals.ConstName (Label, Literals.ConstValue),
  getUnnamedData :: Map.Map Literals.ConstValue Label
} deriving (Show, Eq, Ord)

data ASMFunc = ASMFunc {
  func_name :: Label,
  code      :: [ASMLine],
  params    :: [UnlimitedRegister],
  out_reg   :: Maybe UnlimitedRegister
} deriving (Show, Eq, Ord)

data ASMProgram = ASMProgram {
  data_section :: ASMData,
  functions    :: Map.Map Label ASMFunc
} deriving (Show, Eq, Ord)
