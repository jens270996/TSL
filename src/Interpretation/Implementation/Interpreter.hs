module Interpretation.Implementation.Interpreter where

import TSL.AST
import Utils.AST
import Interpretation.Implementation.Computation

interpretProgram :: Program -> Constant -> Constant
interpretProgram program input = undefined



interpretInvolution :: Involution -> Computation ()
interpretInvolution invol =
    let (Involution _ _ stmts) = invol
    in do interpretStatements (init stmts)
          interpretStatement (last stmts)
          reverseInterpretStatements (init stmts)


interpretStatements :: [Statement] -> Computation ()
interpretStatements = undefined

reverseInterpretStatements :: [Statement] -> Computation ()
reverseInterpretStatements = undefined

interpretStatement :: Statement -> Computation ()
interpretStatement (Assign op x e) =
    do c <- get x
       c' <- interpretExpression e
       c'' <- applyRevOp op c c'
       set x c''
interpretStatement (Loop e1 s1 s2 e2) = loopInner True e1 s1 s2 e2
    -- Assign ReversibleOp Variable Expression
    -- |Loop Expression [Statement] [Statement] Expression
    -- |Conditional Expression [Statement] [Statement] Expression
    -- | Replacement Pattern Pattern
    -- | Skip

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

interpretExpression :: Expression -> Computation Constant
interpretExpression (Constant c)= return c
interpretExpression (EVar x) = look x
interpretExpression (Operation op e1 e2) =
    do c1 <- interpretExpression e1
       c2 <- interpretExpression e2
       applyOp op c1 c2

applyRevOp :: ReversibleOp -> Constant -> Constant -> Computation Constant
applyRevOp XorR Nil c2 = return c2
applyRevOp XorR c1 c2 | c1==c2 = return Nil
applyRevOp XorR _ _ = throw "Error in reversible XOR" >> return Nil
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
    -- TODO: Workout how boolean expressions work
    -- |And
    -- |Or
    -- |Gt
    -- |Lt
    -- |Eq
    -- |Neq
    -- |GtEq
    -- |LtEq
bool :: Constant -> Bool
bool = undefined

