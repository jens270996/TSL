module Utils.AST where

import TSL.AST
import Data.List (nub, length)

-- getters for sub structures.
getMain :: Program -> Involution
getMain (Program m _ _ ) = m

involutions :: Program -> [Involution]
involutions (Program main is _) = main:is

procedures :: Program -> [Procedure]
procedures (Program _ _ ps) = ps


-- Input: Program
-- Output: (Involution Identifiers, Procedure Identifiers)
involutionIds:: Program -> [Identifier]
involutionIds (Program main involutions _) =
    let involutionIds = map (\(Involution id _ _ )-> id) involutions
        (Involution mainId _ _) = main
    in (mainId : involutionIds)

procedureIds :: Program -> [Identifier]
procedureIds (Program _ _ procedures) =
    map (\(Procedure id _ _ _)-> id) procedures

statements:: Program -> [Statement]
statements (Program main involutions procedures) =
    let procedureBodies = concat $ map (\(Procedure _ _ body _)-> body) procedures
        involutionBodies = concat $ map (\(Involution _ _ body)-> body) (main:involutions)
    in procedureBodies ++ involutionBodies

symmetricStatements:: Program -> [Statement]
symmetricStatements (Program main involutions _) =
    map (\(Involution _ _ body)-> last body) (main:involutions)

-- all input output patterns + patterns in statements
patterns:: Program -> [Pattern]
patterns (Program main involutions procedures) =
    let procedurePatterns = concat $ map (\(Procedure _ pIn body pOut) -> pIn:(pOut:(concat $ map patternsStatement body))) procedures
        involutionPatterns = concat $ map (\(Involution _ pIn body) -> pIn:(concat $ map patternsStatement body)) (main:involutions)
    in procedurePatterns ++ involutionPatterns


patternsStatement :: Statement -> [Pattern]
patternsStatement s = case s of
    (Assign _ _ _) -> []
    (Loop _ s1 s2 _) -> concat $ map patternsStatement (s1++s2)
    (Conditional _ s1 s2 _) -> concat $ map patternsStatement (s1++s2)
    (Replacement p1 p2) -> [p1,p2]
    Skip -> []

variablesPattern :: Pattern -> [Variable]
variablesPattern (PVar v) = [v]
variablesPattern (PPair p1 p2) = variablesPattern p1 ++ variablesPattern p2
variablesPattern (PConst _) = []
variablesPattern (Involute _ p) = variablesPattern p
variablesPattern (Call _ p) = variablesPattern p
variablesPattern (Uncall _ p) = variablesPattern p

variablesExpression :: Expression -> [Variable]
variablesExpression (EVar v) = [v]
variablesExpression (Operation _ e1 e2) = variablesExpression e1 ++ variablesExpression e2
variablesExpression _ = []




-- General Utils

unique ::(Eq a) => [a] -> Bool
unique l = (length . nub $ l) == length l