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
        , procedureTests
        , involutionTests
        , programTests
        , whitespaceTests
        ]

programTests :: TestTree
programTests = testGroup "Program tests"
    [ parseProgramT "Single involution" "involution f (x.y) x <- y" (Program (Involution "f" (PPair (PVar "x") (PVar "y")) [(Replacement (PVar "x") (PVar "y"))]) [] [])
    , parseProgramT "Multiple involutions and procedues" "involution f (x.y) x <- y  involution f (x.y) x+=y x <- y procedure f (x.y) x+=y x <- y return (x.y) "
                        (Program (Involution "f" (PPair (PVar "x") (PVar "y")) [(Replacement (PVar "x") (PVar "y"))]) [(Involution "f" (PPair (PVar "x") (PVar "y")) [Assign AddR "x" (EVar "y"),Replacement (PVar "x") (PVar "y")])] [(Procedure "f" (PPair (PVar "x") (PVar "y")) [Assign AddR "x" (EVar "y"),Replacement (PVar "x") (PVar "y")] (PPair (PVar "x") (PVar "y")))])
    , parseProgramT "Mangled procedures and involutions" "involution f (x.y) x <- y procedure f (x.y) x+=y x <- y return (x.y) involution f (x.y) x+=y x <- y"
                        (Program (Involution "f" (PPair (PVar "x") (PVar "y")) [(Replacement (PVar "x") (PVar "y"))]) [(Involution "f" (PPair (PVar "x") (PVar "y")) [Assign AddR "x" (EVar "y"),Replacement (PVar "x") (PVar "y")])] [(Procedure "f" (PPair (PVar "x") (PVar "y")) [Assign AddR "x" (EVar "y"),Replacement (PVar "x") (PVar "y")] (PPair (PVar "x") (PVar "y")))])
    , parseProgramFail "Single procedure" "procedure f (x.y) x+=y return (x.y)"
    ]
    where
     parseProgramT = parseSuccessfully pProgram
     parseProgramFail = parseFail pProgram

involutionTests :: TestTree
involutionTests = testGroup "Involution tests"
    [ parseInvolution "Basic involution" "involution f (x.y) x <- y" (Involution "f" (PPair (PVar "x") (PVar "y")) [(Replacement (PVar "x") (PVar "y"))])
    -- parseInvolutionFail "Non time-symmetric body"  "involution f (x.y) x+=y return (x.y)" Removed since this check is defered to wellformedness check.
    , parseInvolution "Multiple statements" "involution f (x.y) x+=y x <- y" (Involution "f" (PPair (PVar "x") (PVar "y")) [Assign AddR "x" (EVar "y"),Replacement (PVar "x") (PVar "y")])
    ]
    where
     parseInvolution = parseSuccessfully pInvolution
     parseInvolutionFail = parseFail pInvolution

procedureTests :: TestTree
procedureTests = testGroup "Procedure tests"
    [ parseProcedure "Basic procedure" "procedure f (x.y) x+=y return (x.y)" (Procedure "f" (PPair (PVar "x") (PVar "y")) [Assign AddR "x" (EVar "y")] (PPair (PVar "x") (PVar "y")))
    , parseProcedure "Multiple statements" "procedure f (x.y) x+=y x <- y return (x.y)" (Procedure "f" (PPair (PVar "x") (PVar "y")) [Assign AddR "x" (EVar "y"),Replacement (PVar "x") (PVar "y")] (PPair (PVar "x") (PVar "y")))
    , parseProcedureFail "Keyword as procedure name" "procedure loop (x.y) x+=y return (x.y)"
    ]
    where
     parseProcedure = parseSuccessfully pProcedure
     parseProcedureFail = parseFail pProcedure

-- symmetricStatementTests :: TestTree
-- symmetricStatementTests = testGroup "Symmetric statement tests"
--     [ parseSymStatementFail "Reversible assignment add" "x+= '3"
--     , parseSymStatement "XOR assignment" "x^= '3" (XorAssign "x" (Constant (Integer 3)))
--     , parseSymStatement "Replacement" "(x.y) <- '(2.3)" (SReplacement (PPair (PVar "x") (PVar "y")) (PConst (CPair (Integer 2) (Integer 3))))
--     , parseSymStatement "Skip" "skip" SSkip
--     ]
--     where
--      parseSymStatement = parseSuccessfully pSymmetricStatement
--      parseSymStatementFail = parseFail pSymmetricStatement

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
    , parseStatement "Simple replacement" "x <- y" (Replacement (PVar "x") (PVar "y"))
    ]
    where
    parseStatement = parseSuccessfully pStatement
    parseStatementFail = parseFail pStatement



patternTests :: TestTree
patternTests = testGroup "Pattern tests"
    [ parsePattern "Variable" "x" (PVar "x")
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

whitespaceTests :: TestTree
whitespaceTests =
    testGroup "Whitespace tests"
    [ parseSuccessfully (keyword  "symbol") "whitespace after symbol" "symbol  " ()
    , parseSuccessfully (keyword  "symbol") "newline after symbol" "symbol    \n" ()
    , parseSuccessfully (keyword  "symbol") "comment after symbol" "symbol  // safasf asfasfasf asf \n" ()
    , parseFail (keyword  "symbol") "line break ends comment" "symbol  // safasf asfasfasf asf \n asdasdasddasd"
    , parseSuccessfully (keyword  "symbol") "spaces after newline" "symbol    \n        " ()
    , parseSuccessfully (keyword  "symbol") "comment after newline" "symbol    \n    // bla blab bal    " ()
    , parseSuccessfully (keyword  "symbol") "multiple comments" "symbol // bla blab bal    \n    // bla blab bal \n // bla blab bal     " ()
    ]
