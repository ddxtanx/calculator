{-|
Module      : StringFuncs
Description : String functions important for calculator parsing.
Copyright   : (c) Garrett Credi (2020)
License     : GPL-3
Maintainer  : garrettcredi@gmail.com
Stability   : stable

This module deals with strings and the different ways to parse and verify them
for the calculator! The most important functions done are parentheses cutting
out (either simply cut out or spanned), chunking into tokens, and validating
whether or not is actually represents a calculable string.
-}
module StringFuncs
(
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
import Failable

-- | Accumulator for parens balanced
parensBalancedSerial :: 
    String -- ^ Section of string still left to parse.
    -> Int  -- ^ Current parentheses level count
    -> Bool -- ^ Whether or not the parentheses are balanced.
parensBalancedSerial [] 0 = True
parensBalancedSerial [] _ = False
parensBalancedSerial (c:cs) i 
    | i < 0         = False
    | c == '('      = parensBalancedSerial cs (i+1)
    | c == ')'      = parensBalancedSerial cs (i-1)
    | otherwise     = parensBalancedSerial cs i

-- | Determines whether the parentheses in a string are balanced.
parensBalanced :: String -> Bool
parensBalanced str = parensBalancedSerial str 0

-- | Accumulator for paren cutting out.
cutOutParenSerial :: String -> Int -> Bool -> String
cutOutParenSerial [] _ _ = ""
cutOutParenSerial _ 0 True = ""
cutOutParenSerial (c:cs) i _= c:rest
    where
        rest = case c of
            '(' -> cutOutParenSerial cs (i+1) True
            ')' -> cutOutParenSerial cs (i-1) True
            _ -> cutOutParenSerial cs i True

-- | Cuts out a section of a string that contains a complete parenthetical
-- | grouping.
cutOutParen :: 
    String -- ^ String to cut parentheses from.
    -> String -- ^ Cut out complete parentheses section.
cutOutParen str = rmParens $ cutOutParenSerial drpParenStr 0 False
    where
        drpParenStr = dropWhile (/= '(') str
        rmParens "" = ""
        rmParens [_] = ""
        rmParens str = tail . init $ str

-- | Trims the outer parentheses from a string if they are present.
trimOuterParens :: String -> String
trimOuterParens "" = ""
trimOuterParens "(" = ""
trimOuterParens ")" = ""
trimOuterParens [x] = [x]
trimOuterParens str
    | head str == '(' && last str == ')'  = tail . init $ str
    | otherwise                           = str

-- | Accumulator for chunkByPredicateUntil
cbpUntilAcc :: 
    (a -> Bool) -- ^ Predicate function
    -> (a -> Bool) -- ^ Trigger function
    -> ([a] -> ([a], [a])) -- ^ Processor function
    -> [a] -- ^ List to operate on
    -> [a] -- ^ Current chunk
    -> [[a]] -- ^ Chunks accumulator
    -> [[a]] -- ^ Fully chunked
cbpUntilAcc _ _ _ [] mem acc = acc ++ [mem]
cbpUntilAcc pred trig prcs (x:xs) mem acc
    | trig x       = 
        cbpUntilAcc pred trig prcs newXs [] (acc ++ mem:[processedXs])
    | pred x       = cbpUntilAcc pred trig prcs xs (mem ++ [x]) acc
    | otherwise    = cbpUntilAcc (not . pred) trig prcs xs [x] (acc ++ [mem])
    where
        (processedXs, newXs) = prcs (x:xs)

-- | Accumulator for simple chunking by predicate.
cbpAcc :: (a -> Bool) -> [a] -> [a] -> [[a]] -> [[a]]
cbpAcc pred = cbpUntilAcc pred (const False) (const ([],[]))

-- | Chunk a list by a predicate, until a trigger is satisfied.
-- | Once the trigger is satisfies, add a new chunk and continue
-- | processing a (possibly) different portion of the string.
chunkByPredicateUntil :: 
    (a -> Bool) -- ^ Predicate function for chunking.
    -> (a -> Bool) -- ^ Trigger function.
    -> ([a] -> ([a], [a]))  -- ^ Method to process a section of the list and
                            -- ^ leave a section of it for further processing.
    -> [a]  -- ^ List to process.
    -> [[a]] -- ^ Chunked list
chunkByPredicateUntil pred trig prcs l = cbpUntilAcc pred trig prcs l [] []

-- | Chunk list by a predicate w/o any triggers.
chunkByPredicate :: 
    (a -> Bool) -- ^ Predicate to chunk by.
    -> [a] -- ^ List to chunk.
    -> [[a]] -- ^ Chunked list.
chunkByPredicate f l = cbpAcc f l [] []

-- | Breaks up a string into number, operator, or parenthetical sections.
breakUpString :: String -> [String]
breakUpString = 
    chunkByPredicate (not . (`elem` opChars)) . filter notParenNotSpace
    where
        notParenNotSpace c = c /= '(' && c /= ')' && c /= ' '

-- | Determines if the operators present in a string are valid (i.e. no 
-- | consecutive binary operators like +-).
opsAreValid :: String -> Bool
opsAreValid str = all isValid strBrokenUp && notEndOnOp
    where
        strBrokenUp = breakUpString str
        isValid :: String -> Bool
        isValid str' = not (isOp str') || (length str' == 1)
        notEndOnOp = not . isOp . last $ strBrokenUp
                        
-- | Determines if a string is a valid calculator string. A string is valid
-- | if and only if its parentheses are balanced, its operators are valid
-- | and there are no unknown characters present in it.
-- | TODO: Make noWeridStuff more accurate by checking if any non-operator
-- | strings are present, not just characters.
strIsCalcValid :: String -> Failable Bool 
strIsCalcValid str 
    | not . parensBalanced $ strTrimd = 
        Error "Parentheses are not balanced. From strIsCalcValid."
    | not . opsAreValid $ strTrimd    = 
        Error "Invalid operation sequenced. From strIsCalcValid."
    | not . noWeirdStuff $ strTrimd   = 
        Error "Unfamiliar characters present in string. From strIsCalcValid."
    | otherwise                       = Result True
    where
        strTrimd = filter (/= ' ') str
        noWeirdStuff = 
            not . any 
            (
                \c -> c `notElem` opChars ++ concat unOpSymbs ++ "()" 
                && (not . isNum $ [c])
            )

-- | Applies the cartesian product of two functions with the same domain to an
-- | object.
cartProd :: (a -> b) -> (a -> c) -> a -> (b,c)
cartProd f g x = (f x, g x)

-- | Determine if a string is a number
isNum :: String -> Bool
isNum = isJust . (readMaybe :: String -> Maybe Int)

-- | Determine if a string starts with unary operators.
startsWithOps :: String -> Bool
startsWithOps str = (not . null $ opChunk) && opsNotIn rest
    where
        (opChunk, rest) = spanWhileNotUnOp str
        restParenSection = cutOutParen rest
        opsNotIn str' = 
            (not . any (`elem` opChars) $ rest)
            || rest == '(' : restParenSection ++ [')']

-- | Determine if a string is an operator
isOp :: String -> Bool
isOp = all (`elem` opChars)

-- | Determine if a string is an expression
isExpr :: String -> Bool
isExpr str = not $ isNum str || isOp str 

-- | Takes from a string while its accumulator is not a valid unary operator.
takeWhileNotUnOp :: String -> String
takeWhileNotUnOp str = twnuoAcc str ""
    where
        twnuoAcc [] acc = acc
        twnuoAcc (x:xs) acc = 
            if acc `elem` unOpSymbs || x `notElem` concat unOpSymbs 
            then acc 
            else twnuoAcc xs (acc ++ [x])

-- | Spans from a string while its accumulator is not a valid unary operator.
spanWhileNotUnOp :: String -> (String, String)
spanWhileNotUnOp str = (taken, drop (length taken) str)
    where
        taken = takeWhileNotUnOp str

-- | Chunks a string into chunks that are calculator interpretable tokens.
strToTokenChunks :: String -> [String]
strToTokenChunks str = 
    combUnOps . trim $ 
        cbpUntilAcc isOpChar isParen sliceOutParen str [] []
    where
        isOpChar c = c `elem` opChars
        isParen = (==) '('
        
        cutOutParenSurr s = "(" ++ cutOutParen s ++ ")"
        sliceOutParen str = cartProd 
            id (\s -> drop (length s) str) . cutOutParenSurr 
            $ str

        trim = filter (not . null)

        -- | Combines seperated unary operators and the value they're applied to
        -- | into a single string that contains the operator and the value.
        combUnOps :: [String] -> [String]
        combUnOps [] = []
        combUnOps [x] = [x]
        combUnOps (x:y:rest) = 
            if all (`elem` concat unOpSymbs) x 
            then (x ++ y):combUnOps rest 
            else x: combUnOps (y:rest) 
