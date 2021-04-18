module Variable where

import Debug.Trace

type TmpReg = String
type SReg = String

data VariableTracker = VariableTracker {
    table :: [RegisterAssignment], 
    stringLabels :: [String]
    }
    --([RegisterAssignment], [String], Int) -- variables (registers) & string data labels & if label ids

data RegisterAssignment = 
    Assigned SReg String | -- first is register, 2nd variable
    Unassigned SReg
    deriving Show



allSRegisters :: [SReg]
allSRegisters = ["$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"]

newVarTracker :: [String] -> VariableTracker
newVarTracker dataLabels = VariableTracker { table=map Unassigned allSRegisters, stringLabels=dataLabels }

assignNewVar :: VariableTracker -> String -> VariableTracker
assignNewVar varTrack name = 
    let findFreeAndAssign :: [RegisterAssignment] -> [RegisterAssignment]
        findFreeAndAssign [] = error "Ran out of registers to assign"
        findFreeAndAssign (e@(Assigned _ _):ls) = e:findFreeAndAssign ls
        findFreeAndAssign (Unassigned reg:ls) = Assigned reg name:ls

        newAssigns = findFreeAndAssign $ table varTrack
    in VariableTracker{table=newAssigns, stringLabels=stringLabels varTrack} 

getRegisterMaybe :: String -> VariableTracker -> Maybe SReg 
getRegisterMaybe name VariableTracker{table=vars} =
    let helper :: [RegisterAssignment] -> Maybe SReg
        helper [] = Nothing 
        helper ((Assigned reg varName):ls)
            | varName == name = Just reg
            | otherwise = helper ls
        helper (_:ls) = helper ls
    in helper vars

getRegister :: String -> VariableTracker -> SReg 
getRegister name varTable =
    case getRegisterMaybe name varTable of
        (Just s) -> s
        Nothing -> error $ "Cannot find variable: " ++ name
