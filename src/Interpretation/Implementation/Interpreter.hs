module Interpretation.Implementation.Interpreter where

import TSL.AST
import Inversion.Inverter (invertStatements)
import Interpretation.Implementation.Computation
import Utils.Error (ErrorMonad)

type Interpreter a b = a -> Computation b 
interpretProgram :: Program -> Constant -> ErrorMonad Constant
interpretProgram p input =
    let functionStore = constructInitialStores p
        variableStore = emptyVariableStore
    in case runComputation (interpretMain p input) [] functionStore variableStore of
        Right (c, m,_) | m == emptyVariableStore -> Right c
        Right (_,_,_) -> Left "Non nil variables in environment after executing main."
        Left e -> Left e



interpretMain :: Program -> Constant -> Computation Constant
interpretMain (Program main _ _) c =
    let (Involution _id pIn _stmts) = main
    in do deconstruct pIn c
          interpretInvolution main
          construct pIn


interpretInvolution :: Involution -> Computation ()
interpretInvolution invol =
    let (Involution _ _ stmts) = invol
    in do interpretStatements (init stmts)
          interpretStatement (last stmts)
          reverseInterpretStatements (init stmts)


interpretStatements :: [Statement] -> Computation ()
interpretStatements = mapM_ interpretStatement

reverseInterpretStatements :: [Statement] -> Computation ()
reverseInterpretStatements = interpretStatements . invertStatements

interpretStatement :: Statement -> Computation ()
interpretStatement (Assign op x e) =
    do c <- get x
       c' <- interpretExpression e
       c'' <- applyRevOp op c c'
       set x c''
interpretStatement (Loop e1 s1 s2 e2) = loopInner True e1 s1 s2 e2
interpretStatement (Conditional e1 s1 s2 e2) =
    do c1 <- interpretExpression e1
       if bool c1
        then interpretStatements s1
        else interpretStatements s2
       c2 <- interpretExpression e2
       if bool c1 == bool c2
        then return ()
        else throw $ "Assertion in conditional failed. Expected " ++ show (bool c1) ++ " but was " ++ show (bool c2) ++ " condition 2: " ++ show e2
interpretStatement (Replacement p1 p2) =
    do c <- construct p2
       deconstruct p1 c
interpretStatement Skip = return ()

interpretExpression :: Expression -> Computation Constant
interpretExpression (Constant c)= return c
interpretExpression (EVar x) = look x
interpretExpression (Operation op e1 e2) =
    do c1 <- interpretExpression e1
       c2 <- interpretExpression e2
       applyOp op c1 c2

construct :: Pattern -> Computation Constant
construct (PVar var) = get var
construct (PPair p1 p2) =
    do c1 <- construct p1
       c2 <- construct p2
       return $ CPair c1 c2
construct (PConst c) = return c
construct (Involute name p) =
    do c1 <- construct p
       trace $ "RHS involute: " ++ name ++ show p ++ "=" ++ show c1
       involution <- getInvolution name
       Involution _ pIn _ <- getInvolution name
       oldEnv <- getEnvironment
       withEnvironment emptyVariableStore
       deconstruct pIn c1
       interpretInvolution involution
       c <- construct pIn
       assertEnvironmentEmpty name
       withEnvironment oldEnv
       return c
construct (Call name p) =
    do c1 <- construct p
       trace $ "RHS call: " ++ name ++ " " ++ show p ++ "=" ++ show c1
       (Procedure _ pIn s pOut) <- getProcedure name
       oldEnv <- getEnvironment
       withEnvironment emptyVariableStore
       deconstruct pIn c1
       interpretStatements s
       c <- construct pOut
       trace $ "return from RHS call: " ++ name ++ " " ++ show pOut ++ "=" ++ show c
       assertEnvironmentEmpty (name ++ " RHS call ")
       withEnvironment oldEnv
       return c
construct (Uncall name p) =
    do c1 <- construct p
       trace $ "RHS uncall: "++ name ++ " " ++ show p ++ "=" ++ show c1
       (Procedure _ pIn s pOut) <- getProcedure name
       oldEnv <- getEnvironment
       withEnvironment emptyVariableStore
       deconstruct pOut c1
       reverseInterpretStatements s
       c <- construct pIn
       trace $ "return from RHS uncall: " ++ name ++ " " ++ show pIn ++ "=" ++ show c
       assertEnvironmentEmpty name
       withEnvironment oldEnv
       return c
deconstruct :: Pattern -> Constant -> Computation ()
deconstruct (PVar var) c = set var c
deconstruct (PPair p1 p2) (CPair c1 c2) = deconstruct p1 c1 >> deconstruct p2 c2
deconstruct (PPair p1 p2) c = throw $ "attempting to deconstruct non-pair constant into pattern pair: (" ++ show p1 ++ "." ++ show p2 ++ ") <- " ++ show c
deconstruct (PConst c) c1 | c == c1 = return ()
deconstruct (PConst c) c1 = throw $ "Deconstruction of constant " ++ show c1 ++ " into " ++ show c ++ " is invalid since they are not equal"
deconstruct (Involute name p) c =
    do trace $ "LHS involute: " ++ name ++ " " ++ show p ++ "=" ++ show c
       outerEnv <- getEnvironment
       withEnvironment emptyVariableStore
       Involution _ pIn _ <- getInvolution name
       involution <- getInvolution name
       deconstruct pIn c
       interpretInvolution involution
       c' <- construct pIn
       assertEnvironmentEmpty name
       withEnvironment outerEnv
       deconstruct p c'
deconstruct (Call name p) c =
    do outerEnv <- getEnvironment
       withEnvironment emptyVariableStore
       Procedure _ pIn s pOut <- getProcedure name
       trace $ "LHS call: " ++ name ++ " " ++ show pOut ++ "=" ++ show c
       deconstruct pOut c
       reverseInterpretStatements s
       c' <- construct pIn
       assertEnvironmentEmpty name
       withEnvironment outerEnv
       deconstruct p c'
deconstruct (Uncall name p) c =
    do outerEnv <- getEnvironment
       withEnvironment emptyVariableStore
       Procedure _ pIn s pOut <- getProcedure name
       trace $ "LHS uncall: " ++ name ++ " " ++ show pIn ++ "=" ++ show c
       deconstruct pIn c
       interpretStatements s
       c' <- construct pOut
       assertEnvironmentEmpty name
       withEnvironment outerEnv
       deconstruct p c'

loopInner :: Bool -> Expression -> [Statement] -> [Statement] -> Expression -> Computation ()
loopInner b e1 s1 s2 e2 =
    do c1 <- interpretExpression e1
       if bool c1 == b
       then do interpretStatements s1
               c2 <- interpretExpression e2
               if bool c2
               then return ()
               else interpretStatements s2 >> loopInner False e1 s1 s2 e2
       else throw $ "Failed from assertion, should be: " ++ show b


applyRevOp :: ReversibleOp -> Constant -> Constant -> Computation Constant
applyRevOp XorR Nil c2 = return c2
applyRevOp XorR c1 c2 | c1==c2 = return Nil
applyRevOp XorR c1 c2 = (throw $ "Error in reversible XOR: " ++ show c1 ++ "^=" ++ show c2)  >> return Nil
applyRevOp AddR (Integer i1) (Integer i2) = return (Integer (i1+i2))
applyRevOp AddR _ _ = throw "Error in reversible Add" >> return Nil
applyRevOp SubR (Integer i1) (Integer i2) = return (Integer (i1-i2))
applyRevOp SubR _ _ = throw "Error in reversible Sub" >> return Nil

applyOp :: Op -> Constant -> Constant -> Computation Constant
applyOp Xor Nil c2 = return c2
applyOp Xor c1 c2 | c1==c2 = return Nil
applyOp Xor _ _ = throw "Error in XOR" >> return Nil
applyOp Add (Integer i1) (Integer i2) = return (Integer (i1+i2))
applyOp Add _ _ = throw "Error in Add" >> return Nil
applyOp Sub (Integer i1) (Integer i2) = return (Integer (i1-i2))
applyOp Sub _ _ = throw "Error in Sub" >> return Nil
applyOp Mult (Integer i1) (Integer i2) = return (Integer (i1-i2))
applyOp Mult _ _ = throw "Error in Sub" >> return Nil
applyOp Div (Integer i1) (Integer i2) = return (Integer (i1 `div` i2))
applyOp Div _ _ = throw "Error in Sub" >> return Nil
applyOp Mod (Integer i1) (Integer i2) = return (Integer (i1 `mod` i2))
applyOp Mod _ _ = throw "Error in Sub" >> return Nil
applyOp And c1 c2 = return $ if bool c1 then c2 else c1
applyOp Or c1 c2 = return $ if bool c1 then c1 else c2
applyOp Gt (Integer i1) (Integer i2) = return $ if i1 > i2 then trueV else falseV
applyOp Lt (Integer i1) (Integer i2) = return $ if i1 < i2 then trueV else falseV
applyOp Eq c1 c2 = return $ if c1 == c2 then trueV else falseV
applyOp Neq c1 c2 = return $ if c1 == c2 then falseV else trueV
applyOp GtEq (Integer i1) (Integer i2) = return $ if i1 >= i2 then trueV else falseV
applyOp LtEq (Integer i1) (Integer i2) = return $ if i1 <= i2 then trueV else falseV
applyOp Gt _ _ = throw "> expects integers." >> return Nil
applyOp Lt c1 c2 = throw ("< expects integers. Got: " ++ show c1 ++ "<" ++ show c2)  >> return Nil
applyOp GtEq _ _ = throw ">= expects integers." >> return Nil
applyOp LtEq _ _ = throw "<= expects integers." >> return Nil

bool :: Constant -> Bool
bool Nil = False
bool _ = True

trueV :: Constant
trueV = Atom "true"

falseV :: Constant
falseV = Nil

