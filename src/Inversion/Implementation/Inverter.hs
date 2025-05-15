module Inversion.Implementation.Inverter where
import TSL.AST

invertStatements :: [Statement] -> [Statement]
invertStatements = reverse . (map invertStatement)

invertStatement :: Statement -> Statement
invertStatement (Assign AddR x e) = Assign SubR x e
invertStatement (Assign SubR x e) = Assign AddR x e
invertStatement (Assign XorR x e) = Assign XorR x e
invertStatement (Loop e1 s1 s2 e2) = Loop e2 (invertStatements s1) (invertStatements s2) e1
invertStatement (Conditional e1 s1 s2 e2) = Conditional e2 (invertStatements s1) (invertStatements s2) e1
invertStatement (Replacement p1 p2) = Replacement p2 p1
invertStatement Skip = Skip