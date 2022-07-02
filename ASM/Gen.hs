module ASM.Gen where

import qualified Data.Map.Strict as Map

import           ASM.Types

import qualified Util.Flattened  as Flattened
import qualified Util.Literals   as Literals


translateFunc :: Flattened.Function2 -> ASMFunc
translateFunc = undefined

translate :: Flattened.Program2 -> ASMProgram
translate (Flattened.Program2 funcs consts code) = undefined

translateCode :: [Flattened.Stmt2] -> [ASMLine]
translateCode = undefined

translateConsts :: Literals.Consts -> ASMData
translateConsts = undefined
