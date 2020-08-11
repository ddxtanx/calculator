module StringFuncsTest 
(
    string_funcs_tests
)
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import StringFuncs

main :: IO()
main = defaultMain string_funcs_tests

string_funcs_tests = testGroup "String Functions Tests" $
    [
        sf_qc_tests, 
        sf_hunit_tests
    ]

-- Quickcheck tests
sf_qc_tests = testGroup "(QuickCheck tests)" $
    [
        prop_trimOuterTrims,
        prop_numIsNum,
        prop_chunkByEvenChunks
    ]

prop_trimOuterTrims = testProperty "TrimOuterParens trims parens" $ 
    \ s -> (trimOuterParens $ "(" ++ s ++ ")") == s

prop_numIsNum = testProperty "Number strings are numbers" $
    \ x -> (isNum $ show (x :: Int))

prop_chunkByEvenChunks = testProperty "Chunking a number list by parity works" $
    \ l' -> all id . map (\ l -> (all even l) || (all odd l)) . (chunkByPredicate even) $ (l' :: [Int])

-- HUnit tests
sf_hunit_tests = testGroup "(HUnit tests)" $ 
    [

    ]