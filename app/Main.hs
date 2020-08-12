module Main
(
    main
) where

import Calculate(calculate)
import Data.Char(toLower)
import Failable
main :: IO ()
main = do
    putStrLn 
        "Please input something to calculate, or enter end to end computations."
    line <- getLine
    let lineLower = map toLower line
    if line == "end" 
    then return ()
    else do
        case calculate line of
            (Error errorStr) -> putStrLn errorStr
            (Result res) -> putStrLn (line ++ "=" ++ show res)
        main

    
