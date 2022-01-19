module Expression where
import AST
import Variable
import Util.Util ( allTRegisters, allARegisters, TReg )
import Data
import Asm
import Debug.Trace

-- no branches in expressions!!
expressionEvalHelper :: [TReg] -> Expr -> VariableTracker -> ([Line], String) -- returns lines and register where result is
expressionEvalHelper _ (Variabl s) varTable = 
    ([], getRegister s varTable)

expressionEvalHelper (register:_) (Immediate v) _ = 
    let code = asmSetToImmediate register v
    in (code, register)

expressionEvalHelper (tmp:_) (Expr sym (Variabl s) (Immediate n)) varTable =
    let reg = getRegister s varTable
    in  (asmDoStuffImmediate sym tmp reg n, tmp)

expressionEvalHelper (tmp:tmpRegLs) (Expr sym (Variabl s) e) varTable =
    let register = getRegister s varTable
        (code, reg) = expressionEvalHelper tmpRegLs e varTable
        codeAdd = asmDoStuffToRegisters sym tmp register reg
    in (code ++ codeAdd, tmp)

-- expressionEvalHelper tmpRegLs@(tmp:_) (Expr sym (Immediate v) e) varTable = --trace ("immediate = " ++ show v) $
--     let (code, reg) = expressionEvalHelper tmpRegLs e varTable
--         (codeAdd, regOut) = (asmDoStuffImmediate sym tmp reg v, tmp)
--     in  (code ++ codeAdd, regOut)

expressionEvalHelper (tmp:tmpRegLs) (Expr sym e1 e2) varTable = --trace "general expr eval helper" $
    let (code1, reg1) = expressionEvalHelper (tmp:tmpRegLs) e1 varTable
        (code2, reg2) = expressionEvalHelper tmpRegLs e2 varTable
        set2 = asmDoStuffToRegisters sym tmp reg1 reg2 -- bug probably?
    in (concat [code1, code2, set2], tmp)

expressionEvalHelper (tmp:tmpRegLs) (FuncExpr fName givenArgs) varTable =
    let codeRegPairs = concat $ zipWith (\argExpr argReg -> expressionSetReg argReg argExpr varTable) givenArgs allARegisters 
        
        fetch = asmSetToRegister tmp "$v0"
    in (codeRegPairs ++ (Instruction "jal" [fName]:fetch), tmp)

expressionEvalHelper regs expr varTable = error $ "Expression not dealt with: expr = " ++ show expr

expressionEval :: Expr -> VariableTracker -> ([Line], String)
expressionEval = expressionEvalHelper allTRegisters

expressionSetReg :: String -> Expr -> VariableTracker -> [Line]
expressionSetReg s (Variabl v) varTable = asmSetToRegister s (getRegister v varTable)
expressionSetReg s (Immediate n) varTable = asmSetToImmediate s n
expressionSetReg s expr varTable = fst $ expressionEvalHelper (s:allTRegisters) expr varTable
