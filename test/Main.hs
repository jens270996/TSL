module Main (main) where

import Test.Tasty
import ParserTests
import InterpreterTests

main :: IO ()
main = defaultMain $ testGroup "TSL tests" [ParserTests.tests, InterpreterTests.tests]
