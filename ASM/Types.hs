module ASM.Types where

import qualified Data.Map        as Map
import           Numeric.Natural (Natural)

import qualified Util.Classes    as Classes


newtype Label = Label { getLabel :: String } deriving (Show, Eq, Ord)
instance Classes.Nameable Label where
  name = getLabel

newtype UnlimitedRegister = UnlimitedRegister { getNum :: Natural } deriving (Show, Eq, Ord)

newtype ASMLiteral = ASMLiteral { getLiteral :: Int } deriving (Show, Eq, Ord)

data SyscallType = PrintInt | PrintString deriving (Show, Eq, Ord)

instance Enum SyscallType where
  toEnum 1 = PrintInt
  toEnum 4 = PrintString
  toEnum _ = error "Unsupported Syscall"

  fromEnum PrintInt    = 1
  fromEnum PrintString = 4

data Instruction =
    Branch UnlimitedRegister UnlimitedRegister Label
  | Binary UnlimitedRegister UnlimitedRegister (Either UnlimitedRegister ASMLiteral)
  | Uniary UnlimitedRegister (Either UnlimitedRegister ASMLiteral)
  | IfStmt UnlimitedRegister [Instruction] Label [Instruction] Label
  | WhileStmt Label UnlimitedRegister [Instruction] Label
  | Jump Label
  | Syscall {
    params      :: [UnlimitedRegister],
    syscallType ::  SyscallType,
    result      :: Maybe UnlimitedRegister }
  deriving (Show, Eq, Ord)


type ASMLine = Either Instruction Label


newtype ASMData = ASMData {
  getASMData :: Map.Map Label String
} deriving (Show, Eq, Ord)

data ASMFunc = ASMFunc {
  func_name :: Label,
  code      :: [ASMLine]
} deriving (Show, Eq, Ord)

data ASMProgram = ASMProgram {
  data_section :: ASMData,
  functions    :: Map.Map Label [()]
} deriving (Show, Eq, Ord)