module Parser
(
    parse
) where

import Ops 
import AbList
import StringFuncs

data Parseable n = Val n | Promise String
    deriving Show
data Token n = OperT OpType | ParseT (Parseable n)
    deriving Show

type TokenSeq n= AbList (Parseable n) OpType

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

strToTokenSeq :: (Read n) => String -> TokenSeq n
strToTokenSeq = toSeq . strToTokens
    where
        toSeq [] = AbNil
        toSeq [ParseT x] = x :/ AbNil
        toSeq (ParseT x:OperT y: rest) = x :/ y :/ toSeq rest

parse :: (Read n) => String -> Expr n
parse str 
    | isNum str         = Const (read str)
    | otherwise         = consumeTokens . strToTokenSeq $ str
        
parsePToken :: (Read n) => Parseable n -> Expr n
parsePToken (Val n) = Const n
parsePToken (Promise str) = parse str

consumeTokens :: (Read n) => TokenSeq n -> Expr n
consumeTokens (start :/ rest) = consumeTokensAcc (parsePToken start) rest

consumeTokensAcc :: (Read n) => Expr n -> AbList OpType (Parseable n) -> Expr n
consumeTokensAcc curExpr AbNil = curExpr
consumeTokensAcc curExpr (o :/ v :/ AbNil) = Op o curExpr (parsePToken v)
consumeTokensAcc curExpr (o1 :/ v :/ o2 :/ tks) 
    | opTypePreced o1 >= opTypePreced o2        = consumeTokensAcc (Op o1 curExpr (parsePToken v)) (o2 :/ tks)
    | otherwise                                 = Op o1 curExpr (consumeTokensAcc (parsePToken v) (o2 :/ tks))
