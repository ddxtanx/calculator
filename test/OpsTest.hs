module OpsTest (
    ops_tests
)
where
    
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ops
import Failable
import Data.Fixed

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
        prop_ExpEval,
        prop_ModEval
    ]

type FailRes = Failable ResultType
prop_NegEval = testProperty "Neg negates constants" $ 
    \x -> (eval $ Un Neg (Const x)) == (Result (-x) :: FailRes)

prop_SuccEval = testProperty "Succ applies successor" $
    \x -> (eval $ Un Succ (Const x)) == (Result $ x + 1 :: FailRes)

prop_AddEval = testProperty "Addition evaluates correctly" $
    \x y -> (eval $ Op Add (Const x) (Const y)) == (Result $ x+y :: FailRes)

prop_SubEval = testProperty "Subtraction evaluates correctly" $
    \x y -> (eval $ Op Sub (Const x) (Const y)) == (Result $ x-y :: FailRes)

prop_MulEval = testProperty "Multiplication evaluates correctly" $
    \x y -> (eval $ Op Mul (Const x) (Const y)) == (Result $ x*y :: FailRes)

prop_DivEval = testProperty "Division evaluates correctly" $
    \x y -> (y /= 0) ==> (eval $ Op Div (Const x) (Const y)) == 
        (Result $ x/y :: FailRes)

testExp :: Int -> Int -> Bool
testExp x y = evaluedVal == Result calcdVal
    where
        xf = fromIntegral x :: ResultType
        yf = fromIntegral y :: ResultType
        evaluedVal = eval $ Op Exp (Const xf) (Const yf)
        calcdVal = xf ** yf
prop_ExpEval = testProperty "Exponentiation evaluates correctly" $
    \x y -> ((x :: Int) /= 0 || (y :: Int) /= 0) ==> testExp x y

testMod :: Int -> Int -> Bool
testMod x y = evaluedVal == Result calcdVal
    where
        xf = fromIntegral x :: ResultType
        yf = fromIntegral y :: ResultType
        evaluedVal = eval $ Op Mod (Const xf) (Const yf)
        calcdVal = fromIntegral $ mod x y
prop_ModEval = testProperty "Remainder evaluates correctly" $
    \x y -> (y :: Int) /= 0 ==> testMod x y



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
        (Op Add (Const 1) (Const 2)) :: FailRes
    )  @?= Result 5.0

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
    ) @?= Result 22.0

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
    ) @?= Result 34

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
    ) @?= Result (-60)