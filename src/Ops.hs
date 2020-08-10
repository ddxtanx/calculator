module Ops
(
    OpType(Add, Sub, Mul, Div),
    UnaryOp(Neg),
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

data UnaryOp = Neg | Succ
    deriving Show

unOps = [Neg, Succ]
data OpType = Add | Sub | Mul | Div | Exp
    deriving Show

ops :: [OpType]
ops = [Add, Sub, Mul, Div, Exp]

data Expr n = Const n | Un UnaryOp (Expr n) | Op OpType (Expr n) (Expr n)
    deriving Show

opTypePreced :: OpType -> Int
opTypePreced Add = 1
opTypePreced Sub = 1
opTypePreced Mul = 2
opTypePreced Div = 2
opTypePreced Exp = 3

unOpApply :: (Floating n) => UnaryOp -> n -> n
unOpApply Neg = negate
unOpApply Succ = (+1)

opTypeEval :: (Floating n) => OpType -> n -> n -> n
opTypeEval Add = (+)
opTypeEval Sub = (-)
opTypeEval Mul = (*)
opTypeEval Div = (/)
opTypeEval Exp = (**)

opChar :: OpType -> Char
opChar Add = '+'
opChar Sub = '-'
opChar Mul = '*'
opChar Div = '/'
opChar Exp = '^'

assocOpType :: Char -> OpType
assocOpType '+' = Add
assocOpType '-' = Sub
assocOpType '*' = Mul
assocOpType '/' = Div
assocOpType '^' = Exp

unOpSym :: UnaryOp -> String
unOpSym Neg = "~"
unOpSym Succ = "#"

assocUnSymb :: String -> UnaryOp
assocUnSymb "~" = Neg
assocUnSymb "#" = Succ

opChars = map opChar ops
unOpSymbs = map unOpSym unOps

eval :: (Floating n) => Expr n -> n
eval (Const x) = x
eval (Un uo x) = unOpApply uo (eval x)
eval (Op o l r) = opTypeEval o (eval l) (eval r)