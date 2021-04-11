module Util where

generateTmpReg :: Int -> String 
generateTmpReg n = "$t" ++ if n >= 0 && n < 10 then show n else error "Ran out of temporary registers"