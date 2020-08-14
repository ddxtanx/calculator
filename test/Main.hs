module Main where
    
import OpsTest
import StringFuncsTest
import ParserTest
import Test.Tasty

tests = [opsTests, stringFuncsTests, parserTests]
main :: IO()
main = defaultMain $ testGroup "All Tests" tests