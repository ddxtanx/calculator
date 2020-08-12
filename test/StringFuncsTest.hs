module StringFuncsTest 
(
    stringFuncsTests
)
where

import StringFuncs

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO()
main = defaultMain stringFuncsTests

stringFuncsTests = testGroup "String Functions Tests"
    [
        sfQcTests, 
        sfHunitTests
    ]

-- Quickcheck tests
sfQcTests = testGroup "(QuickCheck tests)"
    [
        propTrimOuterTrims,
        propNumIsNum,
        propChunkByEvenChunks
    ]

propTrimOuterTrims = testProperty "TrimOuterParens trims parens" $ 
    \ s -> trimOuterParens ("(" ++ s ++ ")") == s

propNumIsNum = testProperty "Number strings are numbers" $
    \ x -> isNum $ show (x :: Int)

propChunkByEvenChunks = testProperty "Chunking a number list by parity works" $
    \ l' -> all (\ l -> all even l || all odd l) . 
    chunkByPredicate even $ (l' :: [Int])

-- HUnit tests
sfHunitTests = testGroup "(HUnit tests)" 
    [

    ]