module ParserTests (tests) where
import TSL.AST
import Parsing.Implementation.Parser
import Test.Tasty
import Test.Tasty.HUnit

parseSuccessfully :: (Eq a, Show a) => Parser a -> TestName -> String -> a -> TestTree
parseSuccessfully parser name input expected = testCase name $ parseString parser input @?= Right expected

parseFail :: (Show a) => Parser a -> TestName -> String -> TestTree
parseFail parser name input = testCase name $
                                case parseString parser input of
                                    Left _ -> return ()
                                    Right e -> assertFailure $ "Failed to assert that input does not parse, got successful parse: " ++ show e

tests::TestTree
tests =
    testGroup "Parsing tests"
        [ expressionTests
        , constantTests
        , patternTests
        , statementTests
        , symmetricStatementTests
        , procedureTests
        , involutionTests
        , programTests
        ]

programTests :: TestTree
programTests = testGroup "Program tests"
    [

    ]
    where
     parseProgramT = parseSuccessfully pProgram
     parseProgramTFail = parseFail pProgram

involutionTests :: TestTree
involutionTests = testGroup "Involution tests"
    [
        -- TODO
    ]
    where
     parseSymStatement = parseSuccessfully pSymmetricStatement
     parseSymStatementFail = parseFail pSymmetricStatement

procedureTests :: TestTree
procedureTests = testGroup "Procedure tests"
    [
        -- TODO
    ]
    where
     parseSymStatement = parseSuccessfully pSymmetricStatement
     parseSymStatementFail = parseFail pSymmetricStatement

symmetricStatementTests :: TestTree
symmetricStatementTests = testGroup "Symmetric statement tests"
    [ parseSymStatementFail "Reversible assignment add" "x+= '3"
    , parseSymStatement "XOR assignment" "x^= '3" (XorAssign "x" (Constant (Integer 3)))
    , parseSymStatement "Replacement" "(x.y) <- '(2.3)" (SReplacement (PPair (PVar "x") (PVar "y")) (PConst (CPair (Integer 2) (Integer 3))))
    , parseSymStatement "Skip" "skip" SSkip
    ]
    where
     parseSymStatement = parseSuccessfully pSymmetricStatement
     parseSymStatementFail = parseFail pSymmetricStatement

statementTests :: TestTree
statementTests = testGroup "Statement tests"
    [ parseStatement "Reversible assignment add" "x+= '3" (Assign AddR "x" (Constant (Integer 3)))
    , parseStatement "Loop statement" "from x='3 do x-='5 loop x+= '1 until x>'4" (Loop (Operation Eq (EVar "x") (Constant (Integer 3)))
                                                                                        [(Assign SubR "x" (Constant (Integer 5)))]
                                                                                        [(Assign AddR "x" (Constant (Integer 1)))]
                                                                                        (Operation Gt (EVar "x") (Constant (Integer 4)))
                                                                                    )
    , parseStatement "Conditional statement" "if x='3 then x-='5 else x+='1 fi x>'4" (Conditional (Operation Eq (EVar "x") (Constant (Integer 3)))
                                                                                        [(Assign SubR "x" (Constant (Integer 5)))]
                                                                                        [(Assign AddR "x" (Constant (Integer 1)))]
                                                                                        (Operation Gt (EVar "x") (Constant (Integer 4)))
                                                                                      )
    , parseStatement "Skip" "skip" Skip
    , parseStatement "Replacement" "(x.y) <- '(2.3)" (Replacement (PPair (PVar "x") (PVar "y")) (PConst (CPair (Integer 2) (Integer 3))))

    ]
    where
    parseStatement = parseSuccessfully pStatement
    parseStatementFail = parseFail pStatement



patternTests :: TestTree
patternTests = testGroup "Pattern tests"
    [ parsePattern "Variable" "var" (PVar "var")
    , parsePattern "Constant" "'42" (PConst (Integer 42))
    , parsePattern "Pair" "(var . '42)" (PPair (PVar "var") (PConst (Integer 42)))
    , parsePattern "Involute" "involute id x" (Involute "id" (PVar "x"))
    , parsePattern "Call" "call id x" (Call "id" (PVar "x"))
    , parsePattern "Uncall" "uncall id x" (Uncall "id" (PVar "x"))

    ]
    where
     parsePattern = parseSuccessfully pPattern
     parsePatternFail = parseFail pPattern

expressionTests :: TestTree
expressionTests = testGroup "Expression tests"
    [ parseExpression "constant" "'hi" (Constant (Atom "hi") )
    , parseExpression "identifier" "var" (EVar "var")
    , parseExpression "operation" "var * '2" (Operation Mult (EVar "var") (Constant (Integer 2))) 
    , parseExpression "precedence * and +" "x + y * '2" (Operation Add (EVar "x") (Operation Mult (EVar "y") (Constant (Integer 2))))
    , parseExpression "precedence + and &&" "x + y && '2" (Operation And (Operation Add (EVar "x") (EVar "y")) (Constant (Integer 2)))
    , parseExpression "precedence && and =" "x && y = '2" (Operation Eq (Operation And (EVar "x") (EVar "y")) (Constant (Integer 2)))
    , parseExpression "precedence over associativity" "'2 = x && y" (Operation Eq (Constant (Integer 2)) (Operation And (EVar "x") (EVar "y")))
    , parseExpression "parentheses over precedence" "('2 = x) && y" (Operation And (Operation Eq (Constant (Integer 2) ) (EVar "x"))   (EVar "y"))
    , parseExpression "parentheses over associativity" "x *( y * '2)" (Operation Mult  (EVar "x") (Operation Mult(EVar "y") (Constant (Integer 2))))
    , parseExpression "associativity *" "x * y * '2" (Operation Mult (Operation Mult (EVar "x") (EVar "y")) (Constant (Integer 2)))
    , parseExpressionFail "associativity =" "x = y = '2"
    ]
    where
     parseExpression = parseSuccessfully pExpression
     parseExpressionFail = parseFail pExpression

constantTests :: TestTree
constantTests = testGroup "Constant tests"
    [ parseConstant "atom" "'hi" (Atom "hi")
    , parseConstant "integer" "'42" (Integer 42)
    , parseConstant "nil" "'nil"  Nil
    , parseConstantFail "missing '" "42"
    ]
    where
     parseConstant = parseSuccessfully constant
     parseConstantFail = parseFail constant