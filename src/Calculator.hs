module Calculator
(

) where

import StringFuncs
import Parser
import Ops(eval)

calculate :: String -> Maybe Float
calculate str 
    | strIsCalcValid str    = Just (eval . parse $ str)
    | otherwise     = Nothing