module UsefulGenerators(
    ExpressionString(..)
) where

import Test.Tasty.QuickCheck(Arbitrary, Gen, elements, arbitrary, choose)
import Ops
import Control.Monad

-- | Convenience type to use throughout section.
type ExprRes = Expr ResultType

-- | Expressions can be arbitrary
instance (Arbitrary a) => Arbitrary (Expr a) where
    arbitrary = arbitraryExpression

instance Arbitrary UnaryOp where
    arbitrary = elements unOps

instance Arbitrary OpType where
    arbitrary = elements ops

newtype ExpressionString = ExpressionString {
    exprStr :: String
}

instance Arbitrary ExpressionString where
    arbitrary = ExpressionString <$>  arbitraryExpressionString

instance Show ExpressionString where
    show = exprStr

arbitraryExpressionString :: Gen String
arbitraryExpressionString = toString <$> (arbitraryExpression :: Gen ExprRes)

arbitraryExpression :: (Arbitrary a) => Gen (Expr a)
arbitraryExpression = do
    level <- choose (1,5)
    arbitraryExpressionWLevels level

-- We can generate arbitrary expressions by appealing to its recursive structure
arbitraryExpressionWLevels :: (Arbitrary a) => Int -> Gen (Expr a)
arbitraryExpressionWLevels 0 = Const <$> arbitrary
    -- Level 0 expressions are just constants.
arbitraryExpressionWLevels i = do
    -- At least on of the arguments will be fully of level i-1 so that the 
    -- expression itself is of level i.
    let l1 = i-1
    l2 <- choose (0,i)

    combOperator <- arbitrary

    let maybeApply = maybeApplyUnOp <=< arbitraryExpressionWLevels
    e1 <- maybeApply l1
    e2 <- maybeApply l2

    ord <- elements ["lr", "rl"]
    let mkExpr = Op combOperator
    if ord == "lr"
    then
        return $ mkExpr e1 e2
    else
        return $ mkExpr e2 e1


maybeApplyUnOp :: Expr a -> Gen (Expr a)
maybeApplyUnOp expr = do
    apply <- elements ["y", "n"]
    if apply == "y"
    then do
        unOpSym <- arbitrary
        return $ Un unOpSym expr
    else
        return expr