module ParserTest(
    parserTests
) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ops
import Parser
import UsefulGenerators

parserTests = testGroup "Parser Tests" [parserQCTests, parserHUnitTests]

-- QuickCheck tests
parserQCTests = testGroup "QuickCheck tests" 
    [
        propUnOpsParseCorrectly,
        propBinOpsParseCorrectly
    ]

funcUnOpsParseCorrectly :: UnaryOp -> ExpressionString -> Bool
funcUnOpsParseCorrectly o exprStrO = createdExpr == parsedExpr
    where
        exprS = exprStr exprStrO
        expr = parse exprS

        createdExpr = Un o <$> expr

        oStr = unOpSym o
        newStr = oStr ++ "(" ++ exprS ++ ")"
        parsedExpr = parse newStr
-- If we append a unary operator to a valid expression, it parses as the
-- unary operator applied to the expression
propUnOpsParseCorrectly = testProperty "Unary operators parse correctly" $
    \o e -> funcUnOpsParseCorrectly o e

funcBinOpsParseCorrectly :: 
    ExpressionString 
    -> OpType 
    -> ExpressionString 
    -> Bool
funcBinOpsParseCorrectly exs1 o exs2 = createdExpr == parsedExpr
    where
        exprS1 = exprStr exs1
        exprS2 = exprStr exs2

        ex1 = parse exprS1
        ex2 = parse exprS2

        createdExpr = Op o <$> ex1 <*> ex2

        oChar = opChar o
        newStr = exprS1 ++ [oChar] ++ exprS2
        parsedExpr = parse newStr

-- If we infix a binary operator between two valid expressions, it parses as
-- the operator applied to the two expressions.
propBinOpsParseCorrectly = testProperty "Binary operators parse correctly" $
    \e1 o e2 -> funcBinOpsParseCorrectly e1 o e2
-- HUnit Tests
parserHUnitTests = testGroup "HUnit Tests" 
    [
        parseTest1,
        parseTest2,
        parseTest3
    ]

parseTest1 = testCase "Parse test 1" $
    parse "1+2" @?= pure (Op Add (Const 1) (Const 2))

parseTest2 = testCase "Parse test 2" $
    parse "~1" @?= pure (Un Neg (Const 1))

parseTest3 = testCase "Parse test 3" $
    parse "2^(~3 - (6*3/4))" @?= pure (
        Op Exp 
        (Const 2) 
        (Op Sub 
            (Un Neg (Const 3))
            (Op Div (Op Mul (Const 6) (Const 3)) (Const 4))
        )
    )

