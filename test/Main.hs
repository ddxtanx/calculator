module Main where
    
import OpsTest
import StringFuncsTest
import Test.Tasty

tests = [opsTests, stringFuncsTests]
main :: IO()
main = defaultMain $ testGroup "All Tests" tests