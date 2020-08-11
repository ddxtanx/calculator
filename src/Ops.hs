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
    OpType(Add, Sub, Mul, Div, Exp),
    UnaryOp(Neg, Succ),
    Expr(Const, Un, Op),
    opTypePreced,
    opTypeEval,
    opChar,
    assocOpType,
    assocUnSymb,
    opChars,
    unOpSymbs,
    eval
) where

-- | Token representing unary operators
data UnaryOp = 
    Neg -- ^ Token for negation
    | Succ -- ^ Token for successor
    deriving Show

unOps :: [UnaryOp]
unOps = [Neg, Succ]

-- | Token representing binary operators
data OpType = 
    Add -- ^ Token for addition
    | Sub -- ^ Token for subtraction
    | Mul  -- ^ Token for multiplication
    | Div  -- ^ Token for division
    | Exp -- ^ Token for exponentiation
    deriving Show

ops :: [OpType]
ops = [Add, Sub, Mul, Div, Exp]

-- | Datatype representing an expression, either a constant number, a
-- | unary operator applied to an expression, or a binary operator
-- | applied to two expressions.
data Expr n = 
    Const n -- ^ A constant number
    | Un UnaryOp (Expr n) -- ^ A unary operator applied to an expression.
    | Op OpType (Expr n) (Expr n) -- ^ A Binary operator applied to two expressions.
    deriving Show

-- | A function associating each binary operator to its precedence.
opTypePreced :: OpType -> Int
opTypePreced Add = 1
opTypePreced Sub = 1
opTypePreced Mul = 2
opTypePreced Div = 2
opTypePreced Exp = 3

-- | A function that applies a unary operator to a number.
unOpApply :: (Floating n) => UnaryOp -> n -> n
unOpApply Neg = negate
unOpApply Succ = (+1)

-- | A function that applies a binary operator to two numbers.
opTypeEval :: (Floating n) => OpType -> n -> n -> n
opTypeEval Add = (+)
opTypeEval Sub = (-)
opTypeEval Mul = (*)
opTypeEval Div = (/)
opTypeEval Exp = (**)

-- | A function that associates each binary operator to its symbol.
opChar :: OpType -> Char
opChar Add = '+'
opChar Sub = '-'
opChar Mul = '*'
opChar Div = '/'
opChar Exp = '^'

-- | A function that associates a character to a binary operator.
assocOpType :: Char -> OpType
assocOpType '+' = Add
assocOpType '-' = Sub
assocOpType '*' = Mul
assocOpType '/' = Div
assocOpType '^' = Exp

-- | A function that associates a unary operator to its symbol.
unOpSym :: UnaryOp -> String
unOpSym Neg = "~"
unOpSym Succ = "#"

-- | A function that associates a symbol to its unary operator.
assocUnSymb :: String -> UnaryOp
assocUnSymb "~" = Neg
assocUnSymb "#" = Succ

-- | The set of all chars that are associated to a binary operator.
opChars :: [Char] 
opChars = map opChar ops

-- | The set of all strings that are associated to a unary operator.
unOpSymbs :: [String]
unOpSymbs = map unOpSym unOps

-- | A function that recursively evaluates an expression to the fractional it represents.
eval :: (Floating n) => Expr n -> n
eval (Const x) = x
eval (Un uo x) = unOpApply uo (eval x)
eval (Op o l r) = opTypeEval o (eval l) (eval r)