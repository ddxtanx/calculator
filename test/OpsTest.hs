module OpsTest (
    ops_tests
)
where
    
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ops

main :: IO ()
main = defaultMain ops_tests

ops_tests = testGroup "Operator Tests" $ [ops_qc_tests, ops_hu_tests]
-- QuickCheck tests

ops_qc_tests = testGroup "(Quickcheck tests)" $
    [
        prop_NegEval, 
        prop_SuccEval, 
        prop_AddEval,
        prop_SubEval,
        prop_MulEval,
        prop_DivEval,
        prop_ExpEval
    ]

prop_NegEval = testProperty "Neg negates constants" $ 
    \x -> (eval $ Un Neg (Const x)) == (-x :: Float)

prop_SuccEval = testProperty "Succ applies successor" $
    \x -> (eval $ Un Succ (Const x)) == (x+1 :: Float)

prop_AddEval = testProperty "Addition evaluates correctly" $
    \x y -> (eval $ Op Add (Const x) (Const y)) == (x+y :: Float)

prop_SubEval = testProperty "Subtraction evaluates correctly" $
    \x y -> (eval $ Op Sub (Const x) (Const y)) == (x-y :: Float)

prop_MulEval = testProperty "Multiplication evaluates correctly" $
    \x y -> (eval $ Op Mul (Const x) (Const y)) == (x*y :: Float)

prop_DivEval = testProperty "Division evaluates correctly" $
    \x y -> (y /= 0) ==> (eval $ Op Div (Const x) (Const y)) == (x/y :: Float)

testExp :: Int -> Int -> Bool
testExp x y = evaluedVal == calcdVal
    where
        xf = fromIntegral x :: Float
        yf = fromIntegral y :: Float
        evaluedVal = eval $ Op Exp (Const xf) (Const yf)
        calcdVal = xf ** yf
prop_ExpEval = testProperty "Exponentiation evaluates correctly" testExp

-- HUnit tests
ops_hu_tests = testGroup "(HUnit tests)" $
    [
        eval_test1,
        eval_test2,
        eval_test3,
        eval_test4
    ]

eval_test1 = testCase "Multi-layer adding/subtracting" $ 
    (
        eval $ Op Add 
        (Op Sub (Const 4) (Const 2)) 
        (Op Add (Const 1) (Const 2)) :: Float
    )  @?= 5.0

eval_test2 = testCase "Multi-layer mixed MDAS" $ 
    (
        eval $ Op Mul
            (
                Op Add 
                    (Const 3) 
                    (Op Mul (Const 2) (Const 4))
            )
            (
                Op Sub
                    (Op Div (Const 6) (Const 2))
                    (Const 1)
            )
    ) @?= 22.0

eval_test3 = testCase "Multi-layer mixed all ops" $ 
    (
        eval $ Op Add
            (
                Op Exp
                    (Const 2)
                    (Op Div (Const 8) (Const 2))
            )
            (
                Op Mul
                    (Op Sub (Const 10) (Const 7))
                    (Const 6)
            )
    ) @?= 34

eval_test4 = testCase "Multi-layer mixed all ops with unary operators" $    
    (
        eval $ Op Add
            (
                Un Neg $ Op Exp
                    (Un Succ (Const 2))
                    (Op Div (Const 8) (Const 2))
            )
            (
                Op Mul
                    (Op Sub (Const 10) (Const 7))
                    (Un Succ (Const 6))
            )
    ) @?= -60