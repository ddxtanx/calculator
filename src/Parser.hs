module Parser
(
    parse
) where

import Ops 
import StringFuncs

data Parseable n = Val n | Promise String
    deriving Show
data Token n = OperT OpType | ParseT (Parseable n)
    deriving Show

toToken :: (Read n) => String -> Token n
toToken str 
    | isNum str         = ParseT (Val (read str))
    | isOp str          = OperT (assocOpType (head str))
    | otherwise         = ParseT (Promise (trimOuterParens str))


strToTokens :: (Read n) => String -> [Token n]
strToTokens = map toToken . strToTokenChunks

parse :: (Read n) => String -> Expr n
parse str 
    | isNum str         = Const (read str)
    | startsWithOps str = Un (assocUnSymb unOpSymb) (parse rest)
    | otherwise         = consumeTokens . strToTokens $ str
    where
        
        (unOpSymb, rest) = spanWhileNotUnOp str
        
parsePToken :: (Read n) => Parseable n -> Expr n
parsePToken (Val n) = Const n
parsePToken (Promise str) = parse str

consumeTokens :: (Read n) => [Token n] -> Expr n
consumeTokens (ParseT start : rest) = consumeTokensAcc (parsePToken start) rest

consumeTokensAcc :: (Read n) => Expr n -> [Token n] -> Expr n
consumeTokensAcc curExpr [] = curExpr
consumeTokensAcc curExpr [OperT o, ParseT v] =  Op o curExpr (parsePToken v)
consumeTokensAcc curExpr (OperT o1 : ParseT v : OperT o2 : tks) 
    | o1Preced >= o2Preced  = consumeTokensAcc (Op o1 curExpr (parsePToken v)) (OperT o2 : tks)
    | otherwise             = consumeTokensAcc combinedExpr restTokens
    where
        o1Preced = opTypePreced o1
        o2Preced = opTypePreced o2

        getAllHighOpTokens higher [] = (higher, [])
        getAllHighOpTokens higher (OperT op1 : ParseT v1 : rest) = 
            if opTypePreced op1 >= o2Preced 
            then getAllHighOpTokens (higher ++ [OperT op1, ParseT v1]) rest 
            else (higher, OperT op1 : ParseT v1 : rest)

        (higherOpers, restTokens) = getAllHighOpTokens [] (OperT o2 : tks) 
        rightOp = consumeTokensAcc (parsePToken v) higherOpers
        
        combinedExpr = Op o1 curExpr rightOp
