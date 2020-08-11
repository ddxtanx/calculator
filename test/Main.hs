module Main where
    
import Test.Tasty
import OpsTest
import StringFuncsTest

tests = [ops_tests, string_funcs_tests]
main :: IO()
main = defaultMain $ testGroup "All Tests" tests
