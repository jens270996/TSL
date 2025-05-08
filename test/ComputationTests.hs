module ComputationTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Interpretation.Implementation.Computation
import qualified Data.Map as Map
import TSL.AST

someInvolution :: Involution
someInvolution = Involution "i1" (PVar "x") []

someProcedure :: Procedure
someProcedure = Procedure "r1" (PVar "x") [] (PVar "x")

tests::TestTree
tests = testGroup "Computation tests"
        [ testCase "throw" $ runComputation (throw "some error") [] (Map.empty,Map.empty) Map.empty @?= Left "Environment contained: \nfromList []\nCallstack contained: \n[]\n\nCaught error: some error\n"
        , testCase "get involution" $ runComputation (getInvolution "i1") [] (Map.empty,Map.singleton "i1" someInvolution) Map.empty @?= Right (someInvolution,Map.empty,[])
        , testCase "get undefined involution" $ runComputation (getInvolution "i2") [] (Map.empty,Map.singleton "i1" someInvolution) Map.empty @?= Left "Trying to lookup undeclared involution: i2"
        , testCase "get procedure" $ runComputation (getProcedure "r1") [] (Map.singleton "r1" someProcedure, Map.empty) Map.empty @?= Right (someProcedure,Map.empty,[])
        , testCase "get undefined procedure" $ runComputation (getProcedure "r2") [] (Map.singleton "r1" someProcedure,Map.singleton "i1" someInvolution) Map.empty @?= Left "Trying to lookup undeclared procedure: r2"
        , testCase "set uninitialized variable" $ runComputation (set "x" (Integer 2)) [] (Map.singleton "r1" someProcedure,Map.singleton "i1" someInvolution) Map.empty @?= Right ((), Map.singleton "x" (Integer 2), [])
        -- what should behavior be here?
        , testCase "set initialized variable (destructive)"
                $ runComputation (set "x" (Integer 2)) []
                                 (Map.singleton "r1" someProcedure,Map.singleton "i1" someInvolution)
                                 (Map.singleton "x" (Integer 1))
                                        @?= Left "Trying to perform destructive assignment of variable: x"
        , testCase "get variable" $ runComputation (get "x" ) [] (Map.empty,Map.empty) (Map.singleton "x" (Integer 2)) @?= Right (Integer 2,Map.empty,[] )
        , testCase "look variable" $ runComputation (look "x" ) [] (Map.empty,Map.empty) (Map.singleton "x" (Integer 2)) @?= Right (Integer 2,Map.singleton "x" (Integer 2),[] )
        , testCase "get uninitialized variable" $ runComputation (get "x" ) [] (Map.empty,Map.empty) Map.empty @?= Right (Nil,Map.empty, [])
        , testCase "look uninitialized variable" $ runComputation (look "x" ) [] (Map.empty,Map.empty) Map.empty @?= Right (Nil,Map.empty,[])
        , testCase "reset variable" $ runComputation (reset "x" ) [] (Map.empty,Map.empty) (Map.singleton "x" (Integer 2)) @?= Right ((),Map.empty,[])
        , testCase "set variable to Nil" $ runComputation (set "x" Nil ) [] (Map.empty,Map.empty) (Map.singleton "x" (Integer 2)) @?= Right ((),Map.empty,[])
        , testCase "override uninitialized variable" $ runComputation (override "x" (Integer 2)) [] (Map.singleton "r1" someProcedure,Map.singleton "i1" someInvolution) Map.empty @?= Right ((), Map.singleton "x" (Integer 2),[])
        , testCase "override initialized variable (destructive)" $ runComputation (override "x" (Integer 2)) [] (Map.singleton "r1" someProcedure,Map.singleton "i1" someInvolution) (Map.singleton "x" (Integer 1)) @?= Right ((), Map.singleton "x" (Integer 2),[])

        ]