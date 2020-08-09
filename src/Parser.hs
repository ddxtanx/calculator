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
    | otherwise         = ParseT (Promise str)
    where
        assocOpType '+' = Add
        assocOpType '-' = Sub
        assocOpType '*' = Mul
        assocOpType '/' = Div


strToTokens :: (Read n) => String -> [Token n]
strToTokens = map toToken . strToTokenChunks

parse :: (Read n) => String -> Expr n
parse str 
    | isNum str         = Const (read str)
    | otherwise         = consumeTokens . strToTokens $ str
        
parsePToken :: (Read n) => Parseable n -> Expr n
parsePToken (Val n) = Const n
parsePToken (Promise str) = parse str

consumeTokens :: (Read n) => [Token n] -> Expr n
consumeTokens (ParseT start: rest) = consumeTokensAcc (parsePToken start) rest

consumeTokensAcc :: (Read n) => Expr n -> [Token n] -> Expr n
consumeTokensAcc curExpr [] = curExpr
consumeTokensAcc curExpr [OperT o, ParseT v] = Op o curExpr (parsePToken v)
consumeTokensAcc curExpr (OperT o1:ParseT v:OperT o2:tks) 
    | opTypePreced o1 >= opTypePreced o2        = consumeTokensAcc (Op o1 curExpr (parsePToken v)) (OperT o2:tks)
    | otherwise                                 = Op o1 curExpr (consumeTokensAcc (parsePToken v) (OperT o2:tks))
