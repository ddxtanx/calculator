{-|
Module      : Ops
Description : Constructors and functions to work with unary and binary operators.
Copyright   : (c) Garrett Credi (2020)
License     : GPL-3
Maintainer  : garrettcredi@gmail.com
Stability   : stable

This module helps the calculator deal with operations in a functional manner
by providing recursive datatypes and functions that represent expressions and 
the different ways in which operators can be applied to them. To add custom  
operators just add the token you want to represent it to either UnaryOp or 
OpType, if it is a binary operator, add its precedence in, add its evaluation 
function and then finish with the character associations!
-}
module Ops
(
    OpType(..),
    UnaryOp(..),
    Expr(..),
    ResultType,
    opTypePreced,
    opTypeEval,
    opChar,
    assocOpType,
    assocUnSymb,
    unOps,
    ops,
    opChars,
    unOpSym,
    unOpSymbs,
    toString,
    eval
) where

import Failable
import Data.Fixed
import Control.Monad
-- | Token representing unary operators
data UnaryOp = 
    Neg -- ^ Token for negation
    | Succ -- ^ Token for successor
    deriving (Eq, Show)

unOps :: [UnaryOp]
unOps = [Neg, Succ]

-- | Token representing binary operators
data OpType = 
    Add -- ^ Token for addition
    | Sub -- ^ Token for subtraction
    | Mul  -- ^ Token for multiplication
    | Div  -- ^ Token for division
    | Exp -- ^ Token for exponentiation
    | Mod -- ^ Token for modulo
    deriving (Eq, Show)

ops :: [OpType]
ops = [Add, Sub, Mul, Div, Exp, Mod]

-- | Datatype representing an expression, either a constant number, a
-- | unary operator applied to an expression, or a binary operator
-- | applied to two expressions.
data Expr n = 
    Const Int -- ^ A constant number
    | Un UnaryOp (Expr n) -- ^ A unary operator applied to an expression.
    | Op OpType (Expr n) (Expr n) -- ^ A Binary operator applied to two expressions.
    deriving (Eq, Show)

-- | A function associating each binary operator to its precedence.
opTypePreced :: OpType -> Int
opTypePreced Add = 1
opTypePreced Sub = 1
opTypePreced Mul = 2
opTypePreced Div = 2
opTypePreced Exp = 3
opTypePreced Mod = 4

-- | This determines the type of the result that the calculator returns.
type ResultType = Double
-- | A function that applies a unary operator to a number.
unOpApply :: UnaryOp -> ResultType -> ResultType
unOpApply Neg = negate
unOpApply Succ = (+1)

-- | A function that applies a binary operator to two numbers.
opTypeEval :: OpType -> ResultType -> ResultType -> Failable ResultType
opTypeEval Add x y = Result $ x + y
opTypeEval Sub x y = Result $ x - y
opTypeEval Mul x y = Result $ x * y
opTypeEval Div x 0 = Error "Division by zero not allowed. From opTypeEval."
opTypeEval Div x y = Result $ x/y
opTypeEval Exp 0 0 = Error "0^0 is not defined. From opTypeEval."
opTypeEval Exp x y = 
    if x < 0 && (not . isWhole) y
    then Error 
        "Fractional exponent of negative number is not real. From opTypeEval."
    else Result $ x**y
    where
        isWhole n = n == fromInteger (round n)
opTypeEval Mod x y = 
    if y == 0
    then Error "Cannot take remained w.r.t zero. From opTypeEval."
    else Result $ mod' x y
                    

-- | A function that associates each binary operator to its symbol.
opChar :: OpType -> Char
opChar Add = '+'
opChar Sub = '-'
opChar Mul = '*'
opChar Div = '/'
opChar Exp = '^'
opChar Mod = '%'

-- | A function that associates a character to a binary operator.
assocOpType :: Char -> Failable OpType
assocOpType '+' = pure Add
assocOpType '-' = pure Sub
assocOpType '*' = pure Mul
assocOpType '/' = pure Div
assocOpType '^' = pure Exp
assocOpType '%' = pure Mod
assocOpType c = Error $ c : " has no associated operator. From assocOpType."

-- | A function that associates a unary operator to its symbol.
unOpSym :: UnaryOp -> String
unOpSym Neg = "~"
unOpSym Succ = "#"

-- | A function that associates a symbol to its unary operator.
assocUnSymb :: String -> Failable UnaryOp
assocUnSymb "~" = pure Neg
assocUnSymb "#" = pure Succ
assocUnSymb str = 
    Error $ str ++ " has no associated operator. From assocUnSymb."

-- | The set of all chars that are associated to a binary operator.
opChars :: String 
opChars = map opChar ops

-- | The set of all strings that are associated to a unary operator.
unOpSymbs :: [String]
unOpSymbs = map unOpSym unOps


toString :: (Show a) => Expr a -> String
toString (Const x) 
    | x < 0 = "~" ++ show (-x)
    | otherwise = show x
toString (Op o l r) = "(" ++ toString l ++ [opChar o] ++ toString r ++ ")"
toString (Un o e) = unOpSym o ++ toString e

-- | A function that recursively evaluates an expression to the fractional it represents.
eval :: Expr ResultType -> Failable ResultType
eval (Const x) = pure . fromIntegral $ x
eval (Un uo x) = unOpApply uo <$> eval x
eval (Op o l r) = do
    lv <- eval l
    rv <- eval r
    opTypeEval o lv rv