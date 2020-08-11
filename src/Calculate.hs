{-|
Module      : Calculate
Description : Calculates the value of a string.
Copyright   : (c) Garrett Credi (2020)
License     : GPL-3
Maintainer  : garrettcredi@gmail.com
Stability   : stable

This module only exposes enough information to be used as the calculator. 
No internal logic is exposed, and errors (hopefully) cannot arise from usage
of calculator.
-}
module Calculate
(
    calculate
) where

import StringFuncs
import Parser
import Ops(eval)

-- | Calculates the value of a string, if it is valid. If not, nothing is 
-- | returned.
calculate :: String -> Maybe Float
calculate str 
    | strIsCalcValid str    = Just (eval . parse $ str)
    | otherwise     = Nothing