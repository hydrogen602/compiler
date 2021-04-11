module Optimizer where

import Asm

-- optimize :: [Line] -> [Line]
-- optimize [] = []
-- optimize (this@(Instruction "li" [reg, literal]):next@(Instruction "move" [reg1, reg2]):ls)
--     | reg == reg2 = Instruction "li" [reg1, literal]:optimize ls
--     | otherwise = this:optimize (next:ls)
-- optimize (this:ls) = this:optimize ls

optimize :: [Line] -> [Line]
optimize [] = []
optimize ls = ls
