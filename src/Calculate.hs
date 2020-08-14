{-|
Module      : Calculate
Description : Calculates the value of a string.
Copyright   : (c) Garrett Credi (2020)
License     : GPL-3
Maintainer  : garrettcredi@gmail.com
Stability   : stable

This module only exposes enough information to be used as the calculator. No 
internal logic is exposed, and errors cannot arise from usage of calculator.
-}
module Calculate
(
    calculate
) where

import StringFuncs
import Parser
import Failable
import Ops(eval, ResultType)
import Control.Monad

-- | Calculates the value of a string, if it is valid. If not, nothing is 
-- | returned.
calculate :: String -> Failable ResultType
calculate = eval <=< parse