module WellformedTests where

import TSL.AST
import Wellformedness.Implementation.Wellformed
import Test.Tasty
import Test.Tasty.HUnit


testPositive :: (Eq a, Show a) => (a -> Wellformed) -> TestName -> a -> TestTree
testPositive asserter name input = testCase name $ asserter input @?= Nothing

testNegative :: (Show a) => (a -> Wellformed) -> TestName -> a -> TestTree
testNegative asserter name input = testCase name $
                                case asserter input of
                                    Just _ -> return ()
                                    Nothing -> assertFailure $ "Failed to assert failure."

tests::TestTree
tests =
    testGroup "Welformedness tests"
        [ programTests
        , patternTests
        , statementTests
        , symmetricReplacementTests
        , symmetricStatementTests
        , involutionTests
        ]

programTests:: TestTree
programTests =
    testGroup "Program tests"
        [
            -- non unique ids
            -- main is not an involution
            -- call on involution
            -- involute on procedure
            
        ]

symmetricStatementTests :: TestTree
symmetricStatementTests =
    testGroup "Symmetric statements tests"
        [ symmetricStatementPositiveTest "XOR-assignment" (Assign XorR "x" (EVar "y"))
        , symmetricStatementNegativeTest "Reversible assignment" (Assign AddR "x" (EVar "y"))
        , symmetricStatementPositiveTest "Skip" (Skip)
        , symmetricStatementNegativeTest "Loop statement" (Loop (EVar "y") [] [] (EVar "y"))
        , symmetricStatementNegativeTest "Conditional statement" (Conditional (EVar "y") [] [] (EVar "y"))
        ]
        where
            symmetricStatementPositiveTest = testPositive wellformedSymmetricStatement
            symmetricStatementNegativeTest = testNegative wellformedSymmetricStatement


patternTests:: TestTree
patternTests =
    testGroup "Pattern tests"
        [ patternPositiveTest "Distinct vars in pattern" (PPair (PVar "x") (PVar "y"))
        , patternNegativeTest "Dublicate vars in pattern" (PPair (PVar "x") (PVar "x"))
        , patternPositiveTest "Call defined procedure" (Call "r1" (PVar "x"))
        , patternPositiveTest "Uncall defined procedure" (Uncall "r1" (PVar "x"))
        , patternPositiveTest "Involute defined involution" (Involute "i1" (PVar "x"))
        , patternNegativeTest "Call defined involution" (Call "i1" (PVar "x"))
        , patternNegativeTest "Uncall defined involution" (Uncall "i1" (PVar "x"))
        , patternNegativeTest "Involute defined procedure" (Involute "r1" (PVar "x"))
        , patternNegativeTest "Call undefined procedure" (Call "bla" (PVar "x"))
        , patternNegativeTest "Uncall undefined procedure" (Uncall "bla" (PVar "x"))
        , patternNegativeTest "Involute undefined involution" (Involute "bla" (PVar "x"))
        , patternPositiveTest "Var" (PVar "x")
        , patternPositiveTest "Const" (PConst Nil)
        ]
        where
            patternPositiveTest = testPositive (wellformedPattern ["i1","i2"] ["r1","r2"])
            patternNegativeTest = testNegative (wellformedPattern ["i1","i2"] ["r1","r2"])

symmetricReplacementTests:: TestTree
symmetricReplacementTests =
    testGroup "Symmetric replacement tests"
        [ symmetricReplacementNegativeTest "Dublicate vars in RHS pattern" (Replacement (PVar "y") (PPair (PVar "x") (PVar "x")))
        , symmetricReplacementNegativeTest "Dublicate vars in LHS pattern" (Replacement (PPair (PVar "x") (PVar "x")) (PVar "y"))
        , symmetricReplacementPositiveTest "Permutation" (Replacement (PPair (PVar "x") (PVar "y")) (PPair (PVar "y") (PVar "x")))
        , symmetricReplacementPositiveTest "Involute LHS" (Replacement (Involute "id" (PVar "x"))  (PVar "x"))
        , symmetricReplacementPositiveTest "Involute RHS" (Replacement (PVar "x") (Involute "id" (PVar "x")))
        , symmetricReplacementNegativeTest "Involute non-matching patterns" (Replacement (PVar "y") (Involute "id" (PVar "x")))
        , symmetricReplacementNegativeTest "Call is not allowed" (Replacement (PVar "x") (Call "id" (PVar "x")))
        , symmetricReplacementNegativeTest "Uncall is not allowed" (Replacement (PVar "x") (Uncall "id" (PVar "x")))
        , symmetricReplacementPositiveTest "Permutation & Involute" (Replacement (PPair (PVar "z") (PPair (PVar "x") (PVar "y"))) (PPair (Involute "id" (PVar "z"))(PPair (PVar "y") (PVar "x"))))
        , symmetricReplacementNegativeTest "Involute inside permutation" (Replacement (PPair (PVar "x") (PVar "y")) (PPair (Involute "id" (PVar "y")) (PVar "x")))           
        ]
        where
            symmetricReplacementPositiveTest = testPositive wellformedSymmetricReplacement
            symmetricReplacementNegativeTest = testNegative wellformedSymmetricReplacement


statementTests:: TestTree
statementTests =
    testGroup "Statement tests"
        [ statementPositiveTest "Assignment different variables" (Assign XorR "x" (EVar "y"))
        , statementNegativeTest "Assignment same variable" (Assign XorR "x" (EVar "x"))
        ]
        where
            statementPositiveTest = testPositive wellformedStatement
            statementNegativeTest = testNegative wellformedStatement


involutionTests:: TestTree
involutionTests =
    testGroup "Involution tests"
        [ involutionPositiveTest "Last statement is symmetric" (Involution "id" (PVar "x") [(Assign XorR "x" (EVar "y")),(Assign XorR "x" (EVar "y"))])
        , involutionNegativeTest "Last statement is not symmetric" (Involution "id" (PVar "x") [(Assign XorR "x" (EVar "y")),(Assign AddR "x" (EVar "y"))])
        ]
        where
            involutionPositiveTest = testPositive wellformedInvolution
            involutionNegativeTest = testNegative wellformedInvolution