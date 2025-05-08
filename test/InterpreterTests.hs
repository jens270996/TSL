module InterpreterTests (tests) where

import Test.Tasty
import Interpretation.Implementation.Interpreter
import Interpretation.Implementation.Computation
import TSL.AST
import Test.Tasty.HUnit
import qualified Data.Map as Map


interpretSuccessfully :: (Eq a, Show a, Eq b,Show b) => Interpreter a b -> TestName -> a -> FunctionStore -> VariableStore -> b -> VariableStore -> TestTree
interpretSuccessfully interpreter name input funStoreIn varStoreIn out expectedStore =
    testCase name $  case runComputation (interpreter input) [] funStoreIn varStoreIn of
                      Right (out',store,_) -> (out',store) @?= (out,expectedStore)
                      Left _ -> assertFailure "Expected computation to succeed"

interpretFail :: (Eq a, Show a, Eq b,Show b) => Interpreter a b -> TestName -> a -> FunctionStore -> VariableStore -> TestTree
interpretFail interpreter name input funStoreIn varStoreIn =
    testCase name $  case runComputation (interpreter input) [] funStoreIn varStoreIn of
                        Left _ -> return ()
                        Right e -> assertFailure $ "Failed to assert that interpretation of input throws , got successful evaluation: " ++ show e

tests::TestTree
tests =
    testGroup "Interpreter tests"
    [ expressionTests
    , statementTests
    , programTests
    ]

expressionTests :: TestTree
expressionTests =
    testGroup "Expression tests"
    [ interpretExpressionT "Constant expression yields constant" (Constant (Integer 1)) emptyFunctionStore emptyVariableStore (Integer 1) emptyVariableStore
    , interpretExpressionT "Variable expression yields value"  (EVar "x") emptyFunctionStore environmentXInt1 (Integer 1) environmentXInt1
    , interpretExpressionT "Undeclared Variable expression yields nil"  (EVar "x") emptyFunctionStore emptyVariableStore Nil emptyVariableStore
    , interpretExpressionT "Add operation"  (Operation Add (EVar "x") (EVar "x")) emptyFunctionStore environmentXInt1 (Integer 2) environmentXInt1
    , interpretExpressionF "Add operation on non-integer"  (Operation Add (Constant (Integer 1)) (Constant (Atom "Hi"))) emptyFunctionStore emptyVariableStore
    , interpretExpressionT "And operation True returns RHS" (Operation And (Constant (Integer 1)) (Constant (Atom "Hi"))) emptyFunctionStore emptyVariableStore
        (Atom "Hi") emptyVariableStore
    , interpretExpressionT "And operation False RHS" (Operation And (Constant (Integer 1)) (Constant Nil)) emptyFunctionStore emptyVariableStore
        Nil emptyVariableStore
    , interpretExpressionT "And operation False LHS" (Operation And (Constant Nil) (Constant (Integer 1))) emptyFunctionStore emptyVariableStore
        Nil emptyVariableStore
    , interpretExpressionT "Or operation False" (Operation Or (Constant Nil) (Constant Nil)) emptyFunctionStore emptyVariableStore
        Nil emptyVariableStore
    , interpretExpressionT "Or operation True LHS" (Operation Or (Constant (Integer 1)) (Constant Nil)) emptyFunctionStore emptyVariableStore
        (Integer 1) emptyVariableStore
    , interpretExpressionT "Or operation True RHS" (Operation Or (Constant Nil) (Constant (Integer 1))) emptyFunctionStore emptyVariableStore
        (Integer 1) emptyVariableStore
    , interpretExpressionT "Eq operation False" (Operation Eq (Constant (Integer 1)) (Constant (Atom "Hi"))) emptyFunctionStore emptyVariableStore
        Nil emptyVariableStore
    , interpretExpressionT "Eq operation True" (Operation Eq (Constant (Integer 1)) (Constant (Integer 1))) emptyFunctionStore emptyVariableStore
        (Atom "true") emptyVariableStore
    , interpretExpressionT "GtEq operation False" (Operation GtEq (Constant (Integer 1)) (Constant (Integer 2))) emptyFunctionStore emptyVariableStore
        Nil emptyVariableStore
    , interpretExpressionT "GtEq operation True" (Operation GtEq (Constant (Integer 1)) (Constant (Integer 1))) emptyFunctionStore emptyVariableStore
        (Atom "true") emptyVariableStore
    , interpretExpressionF "GtEq operation Non-integer argument" (Operation GtEq (Constant (Integer 1)) (Constant (Atom "Hi"))) emptyFunctionStore emptyVariableStore
    ]
    where
        interpretExpressionT = interpretSuccessfully interpretExpression
        interpretExpressionF = interpretFail interpretExpression


statementTests :: TestTree
statementTests =
    testGroup "Statement tests"
    [ interpretStatementT "Assignment Add" (Assign AddR "x" (Constant (Integer 1))) emptyFunctionStore environmentXInt1
        (Map.singleton "x" (Integer 2))
    , interpretStatementF "Assignment Add non-integer LHS" (Assign AddR "x" (Constant (Integer 1))) emptyFunctionStore environmentXAtomHi
    , interpretStatementF "Assignment Add non-integer RHS" (Assign AddR "x" (Constant (Atom "hi"))) emptyFunctionStore environmentXInt1
    , interpretStatementT "Assignment XOR LHS nil" (Assign XorR "x" (Constant (Integer 1))) emptyFunctionStore emptyVariableStore
        environmentXInt1
    , interpretStatementT "Assignment XOR LHS eq RHS" (Assign XorR "x" (Constant (Integer 1))) emptyFunctionStore environmentXInt1
        emptyVariableStore
    , interpretStatementF "Assignment XOR LHS neq RHS" (Assign XorR "x" (Constant (Integer 1))) emptyFunctionStore environmentXAtomHi
    , interpretStatementT "Loop"
        (Loop (Operation Eq (EVar "x") (Constant (Integer 1))) [(Assign AddR "x" (Constant (Integer 2)))] [(Assign SubR "x" (Constant (Integer 1)))] (Operation Gt (EVar "x") (Constant (Integer 3))))
        emptyFunctionStore environmentXInt1
        (Map.singleton "x" (Integer 4))
    , interpretStatementF "Loop invalid initial from assertion"
        (Loop (Operation Eq (EVar "x") (Constant (Integer 2))) [(Assign AddR "x" (Constant (Integer 2)))] [(Assign SubR "x" (Constant (Integer 1)))] (Operation Gt (EVar "x") (Constant (Integer 3))))
        emptyFunctionStore environmentXInt1
    , interpretStatementF "Loop invalid from assertion after first iteration"
        (Loop (Operation Eq (EVar "x") (Constant (Integer 1))) [(Assign AddR "x" (Constant (Integer 1)))] [(Assign SubR "x" (Constant (Integer 1)))] (Operation Gt (EVar "x") (Constant (Integer 3))))
        emptyFunctionStore environmentXInt1
    , interpretStatementT "Conditional true"
        (Conditional (Operation GtEq (EVar "x") (Constant (Integer 1))) [(Assign AddR "x" (Constant (Integer 2)))] [(Assign SubR "x" (Constant (Integer 1)))] (Operation GtEq (EVar "x") (Constant (Integer 2))))
        emptyFunctionStore environmentXInt1
        (Map.singleton "x" (Integer 3))
    , interpretStatementT "Conditional false"
        (Conditional (Operation Gt (EVar "x") (Constant (Integer 1))) [(Assign AddR "x" (Constant (Integer 2)))] [(Assign SubR "x" (Constant (Integer 1)))] (Operation GtEq (EVar "x") (Constant (Integer 2))))
        emptyFunctionStore environmentXInt1
        (Map.singleton "x" (Integer 0))
    , interpretStatementF "Conditional false - failed assertion"
        (Conditional (Operation Gt (EVar "x") (Constant (Integer 1))) [(Assign AddR "x" (Constant (Integer 2)))] [(Assign AddR "x" (Constant (Integer 1)))] (Operation GtEq (EVar "x") (Constant (Integer 2))))
        emptyFunctionStore environmentXInt1
    , interpretStatementF "Conditional true - failed assertion"
        (Conditional (Operation GtEq (EVar "x") (Constant (Integer 1))) [(Assign AddR "x" (Constant (Integer 2)))] [(Assign AddR "x" (Constant (Integer 1)))] (Operation Gt (EVar "x") (Constant (Integer 3))))
        emptyFunctionStore environmentXInt1
    , interpretStatementT "Skip" Skip emptyFunctionStore environmentXInt1
        environmentXInt1
    , interpretStatementF "Replacement x <- y (x != nil)" (Replacement (PVar "x") (PVar "y")) emptyFunctionStore environmentXInt2YInt1
    , interpretStatementT "Replacement x <- y (x = nil)" (Replacement (PVar "x") (PVar "y")) emptyFunctionStore (Map.singleton "y" (Integer 1))
        environmentXInt1
    , interpretStatementT "Replacement x <- '4" (Replacement (PVar "x") (PConst (Integer 4 ))) emptyFunctionStore emptyVariableStore
        (Map.singleton "x" (Integer 4) )
    , interpretStatementT "Replacement '4 <- x (x=4)" (Replacement (PConst (Integer 4 )) (PVar "x")) emptyFunctionStore (Map.singleton "x" (Integer 4))
        emptyVariableStore
    , interpretStatementF "Replacement '4 <- x (x=1)" (Replacement (PConst (Integer 4 )) (PVar "x")) emptyFunctionStore environmentXInt1
    , interpretStatementT "Replacement z <- (x.y)" (Replacement (PVar "z") (PPair (PVar "x") (PVar "y"))) emptyFunctionStore environmentXInt2YInt1
        (Map.singleton "z" (CPair (Integer 2) (Integer 1)))
    , interpretStatementT "Replacement (x.y) <- z (z = (2.1))" (Replacement (PPair (PVar "x") (PVar "y")) (PVar "z")) emptyFunctionStore (Map.singleton "z" (CPair (Integer 2) (Integer 1)))
        environmentXInt2YInt1
    , interpretStatementF "Replacement (x.y) <- z (z = 1)" (Replacement (PPair (PVar "x") (PVar "y")) (PVar "z")) emptyFunctionStore (Map.singleton "z" (Integer 1))
    , interpretStatementT "Replacement y <- call inc y" (Replacement (PVar "y") (Call "inc" (PVar "y"))) functionStore environmentXInt2YInt1
        (Map.insert "y" (Integer 2) environmentXInt2YInt1)
    , interpretStatementT "Replacement y <- uncall inc y" (Replacement (PVar "y") (Uncall "inc" (PVar "y"))) functionStore environmentXInt2YInt1
        (Map.insert "y" (Integer 0) environmentXInt2YInt1)
    , interpretStatementT "Replacement call inc y <- y" (Replacement (Call "inc" (PVar "y")) (PVar "y")) functionStore environmentXInt2YInt1
        (Map.insert "y" (Integer 0) environmentXInt2YInt1)
    , interpretStatementT "Replacement uncall inc y <- y" (Replacement (Uncall "inc" (PVar "y")) (PVar "y")) functionStore environmentXInt2YInt1
        (Map.insert "y" (Integer 2) environmentXInt2YInt1)
    , interpretStatementF "Replacement call undefined y <- y" (Replacement (Uncall "undefined" (PVar "y")) (PVar "y")) functionStore environmentXInt2YInt1
    , interpretStatementT "Replacement involute swap z <- z" (Replacement (Involute "swap" (PVar "z")) (PVar "z")) functionStore (Map.singleton "z" (CPair (Integer 2) (Integer 1)))
        (Map.singleton "z" (CPair (Integer 1) (Integer 2)))
    , interpretStatementT "Replacement z <- involute swap z" (Replacement (PVar "z") (Involute "swap" (PVar "z"))) functionStore (Map.singleton "z" (CPair (Integer 2) (Integer 1)))
        (Map.singleton "z" (CPair (Integer 1) (Integer 2)))
    ]
    where
        interpretStatementT n s f v = interpretSuccessfully interpretStatement n s f v ()
        interpretStatementF = interpretFail interpretStatement


programTests :: TestTree
programTests =
    testGroup "Program tests"
    [
        interpretProgramT "simple program (swap)" ( Program involutionSwap [] [], CPair (Integer 2) (Integer 1)) emptyFunctionStore emptyVariableStore
            (CPair (Integer 1) (Integer 2)) emptyVariableStore
    ]
    where
        interpretProgramT = interpretSuccessfully (uncurry interpretMain)
        interpretProgramF = interpretFail (uncurry interpretMain)
environmentXAtomHi :: VariableStore
environmentXAtomHi = Map.singleton "x" (Atom "hi")

environmentXInt1 :: VariableStore
environmentXInt1 = Map.singleton "x" (Integer 1)


environmentXInt2YInt1 :: VariableStore
environmentXInt2YInt1 = Map.insert "y" (Integer 1) (Map.singleton "x" (Integer 2))


emptyFunctionStore :: FunctionStore
emptyFunctionStore = (Map.empty,Map.empty)

procedureInc :: Procedure
procedureInc = Procedure "inc" (PVar "x") [Assign AddR "x" (Constant (Integer 1))] (PVar "x")

involutionSwap :: Involution
involutionSwap = Involution "swap" (PPair (PVar "x") (PVar "y")) [Replacement (PPair (PVar "x") (PVar "y")) (PPair (PVar "y") (PVar "x")) ]

functionStore :: FunctionStore
functionStore = (Map.singleton "inc" procedureInc, Map.singleton "swap" involutionSwap)