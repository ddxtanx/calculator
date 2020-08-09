module StringFuncs
(
    cutOutParen,
    chunkByPredicate,
    breakUpString,
    strIsCalcValid,
    strToTokenChunks,
    isNum,
    isOp,
    isExpr
) where

import Ops
import Text.Read
import Data.Maybe

parensBalancedSerial :: String -> Int -> Bool
parensBalancedSerial [] 0 = True
parensBalancedSerial [] _ = False
parensBalancedSerial (c:cs) i 
    | i < 0         = False
    | c == '('      = parensBalancedSerial cs (i+1)
    | c == ')'      = parensBalancedSerial cs (i-1)
    | otherwise     = parensBalancedSerial cs i

parensBalanced :: String -> Bool
parensBalanced str = parensBalancedSerial str 0

cutOutParenSerial :: String -> Int -> Bool -> String
cutOutParenSerial [] _ _ = ""
cutOutParenSerial _ 0 True = ""
cutOutParenSerial (c:cs) i _= c:rest
    where
        rest = case c of
            '(' -> cutOutParenSerial cs (i+1) True
            ')' -> cutOutParenSerial cs (i-1) True
            _ -> cutOutParenSerial cs i True
            
cutOutParen :: String -> String
cutOutParen str = rmParens $ cutOutParenSerial drpParenStr 0 False
    where
        drpParenStr = dropWhile (/= '(') str
        rmParens "" = ""
        rmParens [_] = ""
        rmParens str = tail . init $ str

cbpUntilAcc :: (a -> Bool) -> (a -> Bool) -> ([a] -> ([a], [a])) -> [a] -> [a] -> [[a]] -> [[a]]
cbpUntilAcc _ _ _ [] mem acc = acc ++ [mem]
cbpUntilAcc pred trig process (x:xs) mem acc
    | trig x       = cbpUntilAcc pred trig process newXs [] (acc ++ mem:[processedXs])
    | pred x       = cbpUntilAcc pred trig process xs (mem ++ [x]) acc
    | otherwise    = cbpUntilAcc (not . pred) trig process xs [x] (acc ++ [mem])
    where
        (processedXs, newXs) = process (x:xs)

cbpAcc :: (a -> Bool) -> [a] -> [a] -> [[a]] -> [[a]]
cbpAcc pred = cbpUntilAcc pred (const False) (const ([],[]))

chunkByPredicate :: (a -> Bool) -> [a] -> [[a]]
chunkByPredicate f l = cbpAcc f l [] []

breakUpString :: String -> [String]
breakUpString = chunkByPredicate (not . (`elem` opChars)) . filter notParenNotSpace
    where
        notParenNotSpace c = c /= '(' && c /= ')' && c /= ' '

opsAreValid :: String -> Bool
opsAreValid str = all isValid (zip readStr brokenUpStr) && notEndOnOp
    where
        isValid :: (Maybe Int, String) -> Bool
        isValid (Just _, _) = True
        isValid (Nothing, str) = length str == 1

        brokenUpStr = breakUpString str

        readStr = map readMaybe brokenUpStr :: [Maybe Int]

        notEndOnOp = null readStr || isJust (last readStr) 

strIsCalcValid :: String -> Bool
strIsCalcValid str = parensBalanced strTrimd && opsAreValid strTrimd
    where
        strTrimd = filter (/= ' ') str

cartProd :: (a -> b) -> (a -> c) -> a -> (b,c)
cartProd f g x = (f x, g x)

isNum :: String -> Bool
isNum = isJust . (readMaybe :: String -> Maybe Int)

isOp :: String -> Bool
isOp = all (`elem` opChars)

isExpr :: String -> Bool
isExpr str = not $ isNum str || isOp str 

strToTokenChunks :: String -> [String]
strToTokenChunks str = trim $ cbpUntilAcc isOpChar isParen sliceOutParen str [] []
    where
        isOpChar c = c `elem` opChars
        isParen = (==) '('
        
        sliceOutParen str = cartProd id (\s -> drop (length s + 2) str) . cutOutParen $ str

        trim :: [[a]] -> [[a]]
        trim = filter (not . null)
