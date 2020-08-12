module OpsTest (
    opsTests
)
where
    
import Data.Fixed
import Failable
import Ops
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain opsTests

opsTests = testGroup "Operator Tests" [opsQcTests, opsHUTests]
-- QuickCheck tests

opsQcTests = testGroup "(Quickcheck tests)" 
    [
        propNegEval, 
        propSuccEval, 
        propAddEval,
        propSubEval,
        propMulEval,
        propDivEval,
        propExpEval,
        propModEval
    ]

type FailRes = Failable ResultType
propNegEval = testProperty "Neg negates constants" $ 
    \x ->  eval (Un Neg (Const x)) == (Result (-x) :: FailRes)

propSuccEval = testProperty "Succ applies successor" $
    \x -> eval (Un Succ (Const x)) == (Result $ x + 1 :: FailRes)

propAddEval = testProperty "Addition evaluates correctly" $
    \x y -> eval (Op Add (Const x) (Const y)) == (Result $ x+y :: FailRes)

propSubEval = testProperty "Subtraction evaluates correctly" $
    \x y -> eval (Op Sub (Const x) (Const y)) == (Result $ x-y :: FailRes)

propMulEval = testProperty "Multiplication evaluates correctly" $
    \x y -> eval (Op Mul (Const x) (Const y)) == (Result $ x*y :: FailRes)

propDivEval = testProperty "Division evaluates correctly" $
    \x y -> (y /= 0) ==> eval (Op Div (Const x) (Const y)) == 
        (Result $ x/y :: FailRes)

testExp :: Int -> Int -> Bool
testExp x y = evaluedVal == Result calcdVal
    where
        xf = fromIntegral x :: ResultType
        yf = fromIntegral y :: ResultType
        evaluedVal = eval $ Op Exp (Const xf) (Const yf)
        calcdVal = xf ** yf
propExpEval = testProperty "Exponentiation evaluates correctly" $
    \x y -> ((x :: Int) /= 0 || (y :: Int) /= 0) ==> testExp x y

testMod :: Int -> Int -> Bool
testMod x y = evaluedVal == Result calcdVal
    where
        xf = fromIntegral x :: ResultType
        yf = fromIntegral y :: ResultType
        evaluedVal = eval $ Op Mod (Const xf) (Const yf)
        calcdVal = fromIntegral $ mod x y
propModEval = testProperty "Remainder evaluates correctly" $
    \x y -> (y :: Int) /= 0 ==> testMod x y



-- HUnit tests
opsHUTests = testGroup "(HUnit tests)" 
    [
        evalTest1,
        evalTest2,
        evalTest3,
        evalTest4
    ]

evalTest1 = testCase "Multi-layer adding/subtracting" $ 
    (
        eval $ Op Add 
        (Op Sub (Const 4) (Const 2)) 
        (Op Add (Const 1) (Const 2)) :: FailRes
    )  @?= Result 5.0

evalTest2 = testCase "Multi-layer mixed MDAS" $
    eval ( Op Mul
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

evalTest3 = testCase "Multi-layer mixed all ops" $
    eval ( Op Add
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

evalTest4 = testCase "Multi-layer mixed all ops with unary operators" $    
    eval ( Op Add
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
    )@?= Result (-60)