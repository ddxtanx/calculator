module Main
(
    main
) where

import Calculate(calculate)
import Data.Char(toLower)

main :: IO ()
main = do
    putStrLn "Please input something to calculate, or enter end to end computations."
    line <- getLine
    let lineLower = map toLower line
    if line == "end" 
    then return ()
    else do
        case calculate line of
            Nothing -> putStrLn "Your input string was malformed, please try again."
            Just res -> putStrLn (line ++ "=" ++ show res)
        main

    
