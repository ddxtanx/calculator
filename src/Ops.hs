module Ops
(
    OpType(Add, Sub, Mul, Div),
    Expr(Const, Op),
    opTypePreced,
    opTypeEval,
    opChar,
    opChars,
    eval
) where

data OpType = Add | Sub | Mul | Div
    deriving Show
ops = [Add, Sub, Mul, Div]

data Expr n = Const n | Op OpType (Expr n) (Expr n)
    deriving Show

opTypePreced :: OpType -> Int
opTypePreced Add = 1
opTypePreced Sub = 1
opTypePreced Mul = 2
opTypePreced Div = 2

opTypeEval :: (Fractional n) => OpType -> n -> n -> n
opTypeEval Add = (+)
opTypeEval Sub = (-)
opTypeEval Mul = (*)
opTypeEval Div = (/)

opChar :: OpType -> Char
opChar Add = '+'
opChar Sub = '-'
opChar Mul = '*'
opChar Div = '/'

opChars = map opChar ops

eval :: (Fractional n) => Expr n -> n
eval (Const x) = x
eval (Op o l r) = opTypeEval o (eval l) (eval r)