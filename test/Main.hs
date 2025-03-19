module Main (main) where

import Test.Tasty
import ParserTests
import InterpreterTests
import WellformedTests
import ComputationTests

main :: IO ()
main = defaultMain $ testGroup "TSL tests" [ParserTests.tests, InterpreterTests.tests, WellformedTests.tests, ComputationTests.tests]
