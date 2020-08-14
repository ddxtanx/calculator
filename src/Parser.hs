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
import Failable
import StringFuncs
import Control.Monad

-- | A data structure representing values that can be readily parsed into an 
-- | expression.
data Parseable = 
    Val Int -- ^ A plain value.
    | Promise String -- ^ A not-yet-processed promise to parse a string
                     -- ^ i.e. a parenthetical sub-expression.
    deriving Show

-- | A data structure that is either an operator or a parseable that the parser
-- | can parse into an expression
data Token = 
    OperT OpType -- ^ A token representing a binary operator
    | ParseT Parseable -- ^ A token representing a parseable value.
    deriving Show

-- | Reads a token string into the token it represents.
toToken :: String -> Failable Token
toToken "" = Error "Cannot tokenize empty string. From toToken."
toToken str 
    | isNum str         = Result $ ParseT (Val (read str))
    | isOp str          = OperT <$> (assocOpType . head $ str)
    | otherwise         = pure $ ParseT (Promise (trimOuterParens str))

-- | Reads a calculator string into a list of tokens.
strToTokens :: String -> Failable [Token]
strToTokens = mapM toToken . strToTokenChunks

-- | Parses a string into an expression. For nums and parseable values
-- | attached to a unary operator, parsing is done simply recursively, but for
-- | more complicated strings, the string is converted into tokens and the 
-- | tokens are 'compiled' into the expression it represents.
parse :: String -> Failable (Expr ResultType)
parse str 
    | isNum str                     = Const . read <$> str'
    | startsWithOps str             = do
        symb <- assocUnSymb unOpSymb
        restE <- parse rest
        return $ Un symb restE
    | otherwise                     = (consumeTokens <=< strToTokens) =<< str'
    where
        strValid = strIsCalcValid str
        str' = case strValid of
            Result _ -> Result str
            Error err -> Error err
        (unOpSymb, rest) = spanWhileNotUnOp str
        
-- | Parses a parseable token into the expression it represents.
parsePToken :: Parseable -> Failable (Expr ResultType)
parsePToken (Val n) = Result $ Const n
parsePToken (Promise str) = parse str

-- | Consumes a token list into an expression.
consumeTokens :: [Token] -> Failable (Expr ResultType)
consumeTokens [] = Error "Cannot consume empty string. From consumeTokens."
consumeTokens (ParseT start : rest) = do
        pTokenVal <- parsePToken start
        consumeTokensAcc pTokenVal rest
consumeTokens (OperT start : rest) = 
    Error "Tokens cannot start with operator. From consumeTokens."

-- | Accumulator for consumeTokens. This is the heart of the parsing system.
-- | To accurately consume a sequence of tokens into an expression, 3 cases 
-- | are considered: if there are no more tokens to consume, the current 
-- | expression is the compiled expression; if there is just one more
-- | operation to be applied, the operator is applied to the current expression
-- | and to the next value; otherwise the consumer consumes the operators with
-- | precedence in mind. This just means that the higher precedence operators 
-- | are always consumed before the lower precedence operators are.
consumeTokensAcc ::
    Expr ResultType -- ^ The current expression argument.
    -> [Token] -- ^ The list of remaining tokens to consume.
    -> Failable (Expr ResultType) -- ^ The compiled expression.
consumeTokensAcc curExpr [] = Result curExpr
consumeTokensAcc curExpr [OperT o, ParseT v] = fmap 
    (Op o curExpr) 
    (parsePToken v)
consumeTokensAcc curExpr (OperT o1 : ParseT v : OperT o2 : tks) 
    -- If the first operator is of equal or greater precedence than the next, just process as is 
    | o1Preced >= o2Preced  = do
        pTokenVal <- parsePToken v
        consumeTokensAcc 
            (Op o1 curExpr pTokenVal) 
            (OperT o2 : tks)
    -- Otherwise do some fancy processing explained below.  
    | otherwise             = do
        -- This is just the token list, spanned into higher and not higher subsections.
        let (higherOpers, restTokens) = getAllHighOpTokens [] (OperT o2 : tks) 
        pTokenVal <- parsePToken v
        rightOp <- consumeTokensAcc pTokenVal higherOpers

        -- The combined expression is just op1 applied to the expression in memory and the expression parsed from the higher precedence operators.
        let combinedExpr = Op o1 curExpr rightOp
        consumeTokensAcc combinedExpr restTokens
    where
        o1Preced = opTypePreced o1
        o2Preced = opTypePreced o2

         -- Gets all the higher precedence operators succeeding operator 1.
        getAllHighOpTokens higher [] = (higher, [])
        getAllHighOpTokens higher (OperT op1 : ParseT v1 : rest) = 
            if opTypePreced op1 >= o2Preced 
            then getAllHighOpTokens (higher ++ [OperT op1, ParseT v1]) rest 
            else (higher, OperT op1 : ParseT v1 : rest)
consumeTokensAcc _ [OperT _] = 
    Error "String must not end in operator. From consumeTokensAcc."
consumeTokensAcc _ (OperT _ : OperT _ : rest) = Error 
    "Operators must not be consecutive. From consumeTokensAcc."
consumeTokensAcc _ (ParseT _ : ParseT _ : rest) = Error
    "Parseables must not be consecutive. From consumeTokensAcc."
consumeTokensAcc _ _ = 
    Error (
        "Whatever you did, it's so wrong I don't know where to begin... " ++ 
        "From consumeTokensAcc."
    )