{-|
Module      : Parser
Description : Parses a string into an expression.
Copyright   : (c) Garrett Credi (2020)
License     : GPL-3
Maintainer  : garrettcredi@gmail.com
Stability   : stable

This module's purpose is to parse a string into a valid expression. To acheive 
this, a system of interpretable tokens is introduced that allows the parse 
function to serially 'compile' a token sequence into an expression. 
-}
module Parser
(
    parse
) where

import Ops 
import StringFuncs

-- | A data structure representing values that can be readily parsed into an 
-- | expression.
data Parseable n = 
    Val n -- ^ A plain value.
    | Promise String -- ^ A not-yet-processed promise to parse a string
                     -- ^ i.e. a parenthetical sub-expression.
    deriving Show

-- | A data structure that is either an operator or a parseable that the parser
-- | can parse into an expression
data Token n = 
    OperT OpType -- ^ A token representing a binary operator
    | ParseT (Parseable n) -- ^ A token representing a parseable value.
    deriving Show

-- | Reads a token string into the token it represents.
toToken :: (Read n) => String -> Token n
toToken str 
    | isNum str         = ParseT (Val (read str))
    | isOp str          = OperT (assocOpType (head str))
    | otherwise         = ParseT (Promise (trimOuterParens str))

-- | Reads a calculator string into a list of tokens.
strToTokens :: (Read n) => String -> [Token n]
strToTokens = map toToken . strToTokenChunks

-- | Parses a string into an expression. For nums and parseable values
-- | attached to a unary operator, parsing is done simply recursively, but for
-- | more complicated strings, the string is converted into tokens and the 
-- | tokens are 'compiled' into the expression it represents.
parse :: (Read n) => String -> Expr n
parse str 
    | isNum str         = Const (read str)
    | startsWithOps str = Un (assocUnSymb unOpSymb) (parse rest)
    | otherwise         = consumeTokens . strToTokens $ str
    where
        
        (unOpSymb, rest) = spanWhileNotUnOp str
        
-- | Parses a parseable token into the expression it represents.
parsePToken :: (Read n) => Parseable n -> Expr n
parsePToken (Val n) = Const n
parsePToken (Promise str) = parse str

-- | Consumes a token list into an expression.
consumeTokens :: (Read n) => [Token n] -> Expr n
consumeTokens (ParseT start : rest) = consumeTokensAcc (parsePToken start) rest

-- | Accumulator for consumeTokens. This is the heart of the parsing system.
-- | To accurately consume a sequence of tokens into an expression, 3 cases 
-- | are considered: if there are no more tokens to consume, the current 
-- | expression is the compiled expression; if there is just one more
-- | operation to be applied, the operator is applied to the current expression
-- | and to the next value; otherwise the consumer consumes the operators with
-- | precedence in mind. This just means that the higher precedence operators 
-- | are always consumed before the lower precedence operators are.
consumeTokensAcc :: (Read n) => 
    Expr n -- ^ The current expression argument.
    -> [Token n] -- ^ The list of remaining tokens to consume.
    -> Expr n -- ^ The compiled expression.
consumeTokensAcc curExpr [] = curExpr
consumeTokensAcc curExpr [OperT o, ParseT v] =  Op o curExpr (parsePToken v)
consumeTokensAcc curExpr (OperT o1 : ParseT v : OperT o2 : tks) 
    | o1Preced >= o2Preced  = 
        consumeTokensAcc 
            (Op o1 curExpr (parsePToken v)) 
            (OperT o2 : tks)
        -- If the first operator is of equal or greater precedence than the next, just process as is 
    | otherwise             = consumeTokensAcc combinedExpr restTokens
    -- Otherwise do some fancy processing explained below.
    where
        o1Preced = opTypePreced o1
        o2Preced = opTypePreced o2

        -- | Gets all the higher precedence operators succeeding operator 1.
        getAllHighOpTokens higher [] = (higher, [])
        getAllHighOpTokens higher (OperT op1 : ParseT v1 : rest) = 
            if opTypePreced op1 >= o2Preced 
            then getAllHighOpTokens (higher ++ [OperT op1, ParseT v1]) rest 
            else (higher, OperT op1 : ParseT v1 : rest)

        -- This is just the token list, spanned into higher and not higher subsections.
        (higherOpers, restTokens) = getAllHighOpTokens [] (OperT o2 : tks) 
        -- The right operator is now the expression from parsing the higher precedence operator section.
        rightOp = consumeTokensAcc (parsePToken v) higherOpers
        
        -- The combined operator is just op1 applied to the expression in memory and the expression parsed from the higher precedence operators.
        combinedExpr = Op o1 curExpr rightOp
