module Wellformedness.Implementation.Wellformed where
import Utils.AST (statements,variablesPattern,patterns, variablesExpression, unique, involutions, procedureIds, involutionIds)
import Control.Applicative ((<|>))
import Utils.Error (Error)
import TSL.AST


type Wellformed = Maybe Error

wellformedProgram :: Program -> Wellformed
wellformedProgram p =
    let pIds = procedureIds p
        iIds = involutionIds p
    -- TODO: consider order
    in (foldl (<|>) Nothing $ map (wellformedPattern iIds pIds) (patterns p))
        <|> (if unique (iIds++pIds) then Nothing else Just "Multiple declarations of Involution/Procedure with same name.")
        <|> (foldl (<|>) Nothing $ map (wellformedStatement ) (statements p))
        <|> (foldl (<|>) Nothing $ map (wellformedInvolution ) (involutions p))



wellformedInvolution :: Involution -> Wellformed
wellformedInvolution (Involution _ _ stmts) = wellformedSymmetricStatement (last stmts)

wellformedStatement :: Statement -> Wellformed
wellformedStatement (Assign _ var exp) =
    if var `elem` (variablesExpression exp)
    then Just $ "Error variable: "++ var ++ " occurs on both LHS and RHS of assignment."
    else Nothing
wellformedStatement _ = Nothing

wellformedSymmetricStatement :: Statement -> Wellformed
wellformedSymmetricStatement s =
    case s of
        (Assign XorR _ _) -> Nothing
        (Replacement p1 p2) -> wellformedSymmetricReplacement (Replacement p1 p2) 
        (Conditional e1 [s1] [s2] e2) | e1 == e2 ->
            wellformedSymmetricStatement s1 >> wellformedSymmetricStatement s2
        Skip -> Nothing
        _ -> Just "Invalid statement type for symmetric statement"

wellformedSymmetricReplacement :: Statement -> Wellformed
wellformedSymmetricReplacement (Replacement p1 p2) = symmetricReplacement p1 p2
wellformedSymmetricReplacement _ = Just "Symmetric replacement should always be a replacement statement."

symmetricReplacement :: Pattern -> Pattern -> Wellformed
symmetricReplacement p1 p2 | p1==p2 = Nothing
symmetricReplacement (Involute _ q1) q2 | q1==q2 = Nothing
symmetricReplacement q1 (Involute _ q2) | q1==q2 = Nothing
symmetricReplacement (PPair q1 q2) (PPair q1' q2') =
            (symmetricReplacement q1 q1' <|> symmetricReplacement q2 q2')
                >> permutationReplacement (PPair q1 q2) (PPair q1' q2')
symmetricReplacement p1 p2 = Just $ "Replacement is not symmetric: " ++ show p1 ++ show p2

permutationReplacement :: Pattern -> Pattern -> Wellformed
permutationReplacement p1 p2 | p1 == p2 = Nothing
permutationReplacement (PPair q1 q2) (PPair q1' q2') =
            (permutationReplacement q1 q1' <|> permutationReplacement q2 q2')
                >> (permutationReplacement q1 q2' <|> permutationReplacement q2 q1')
permutationReplacement p1 p2 = Just $ "Replacement violates permutation rules "  ++ show p1 ++ show p2

wellformedPattern :: [Identifier] -> [Identifier] -> Pattern -> Wellformed
wellformedPattern involutionIds procedureIds (Call id p) =
    if id `elem` procedureIds
    then wellformedPattern involutionIds procedureIds p
    else Just $ "Called procedure: " ++ id ++ " is not defined"
wellformedPattern involutionIds procedureIds (Uncall id p) =
    if id `elem` procedureIds
    then wellformedPattern involutionIds procedureIds p
    else Just $ "Called procedure: " ++ id ++ " is not defined"
wellformedPattern involutionIds procedureIds (Involute id p) =
    if id `elem` involutionIds
    then wellformedPattern involutionIds procedureIds p
    else Just $ "Called procedure: " ++ id ++ " is not defined"
wellformedPattern involutionIds procedureIds (PPair p1 p2) =
    if unique . variablesPattern $ (PPair p1 p2)
    then wellformedPattern involutionIds procedureIds p1
            <|> wellformedPattern involutionIds procedureIds p2
    else Just "Variables in pattern must be unique."
wellformedPattern _ _ _ = Nothing

-- Checks to do:
-- Unique identifiers


-- Maybe check??
-- Variable assigned to may not occur on right hand side of assignment???