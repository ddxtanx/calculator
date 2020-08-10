module StringFuncs
(
    cutOutParen,
    trimOuterParens,
    chunkByPredicate,
    breakUpString,
    strIsCalcValid,
    strToTokenChunks,
    spanWhileNotUnOp,
    isNum,
    startsWithOps,
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
            
trimOuterParens :: String -> String
trimOuterParens "" = ""
trimOuterParens "(" = ""
trimOuterParens ")" = ""
trimOuterParens [x] = [x]
trimOuterParens str
    | head str == '(' && last str == ')'  = tail . init $ str
    | otherwise                           = str

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
opsAreValid str = all isValid strBrokenUp && notEndOnOp
    where
        strBrokenUp = breakUpString str
        isValid :: String -> Bool
        isValid str' = not (isOp str') || (length str' == 1)
        notEndOnOp = not . isOp . last $ strBrokenUp
                        

strIsCalcValid :: String -> Bool
strIsCalcValid str = parensBalanced strTrimd && opsAreValid strTrimd
    where
        strTrimd = filter (/= ' ') str

cartProd :: (a -> b) -> (a -> c) -> a -> (b,c)
cartProd f g x = (f x, g x)

isNum :: String -> Bool
isNum = isJust . (readMaybe :: String -> Maybe Int)

startsWithOps :: String -> Bool
startsWithOps str = (not . null $ opChunk) && opsNotIn rest
    where
        (opChunk, rest) = spanWhileNotUnOp str
        restParenSection = cutOutParen rest
        opsNotIn str' = (isJust . (readMaybe :: String -> Maybe Int) $ str') || rest == '(' : restParenSection ++ [')']


isOp :: String -> Bool
isOp = all (`elem` opChars)

isExpr :: String -> Bool
isExpr str = not $ isNum str || isOp str 

takeWhileNotUnOp :: String -> String
takeWhileNotUnOp str = twnuoAcc str ""
    where
        twnuoAcc [] acc = acc
        twnuoAcc (x:xs) acc = if acc `elem` unOpSymbs then acc else twnuoAcc xs (acc ++ [x])

spanWhileNotUnOp :: String -> (String, String)
spanWhileNotUnOp str = (taken, drop (length taken) str)
    where
        taken = takeWhileNotUnOp str

strToTokenChunks :: String -> [String]
strToTokenChunks str = combUnOps . trim $ cbpUntilAcc isOpChar isParen sliceOutParen str [] []
    where
        isOpChar c = c `elem` opChars
        isParen = (==) '('
        
        cutOutParenSurr s= '(' : cutOutParen s ++ [')']
        sliceOutParen str = cartProd id (\s -> drop (length s) str) . cutOutParenSurr $ str

        trim = filter (not . null)

        combUnOps :: [String] -> [String]
        combUnOps [] = []
        combUnOps [x] = [x]
        combUnOps (x:y:rest) = if all (`elem` concat unOpSymbs) x then (x ++ y):combUnOps rest else x: combUnOps (y:rest) 
