module ASTPrinting.Implementation.Printer where

import TSL.AST


-- (program . (main . (invols . procs)))
printProgram :: Program -> String
printProgram (Program main invols procs) =
    prepend "program" $
        prepend (printInvol main) $
        prepend (printInvols invols) (printProcs procs)

printInvols :: [Involution] -> String
printInvols = list . (map printInvol)

printProcs :: [Procedure] -> String
printProcs = list . (map printProc)

printStmts :: [Statement] -> String
printStmts = list . (map printStmt)
    
printInvol :: Involution -> String
printInvol (Involution id p stmts) =
    prepend "involution" $
        prepend id $
        prepend (printPattern p) (printStmts stmts)

printProc :: Procedure -> String
printProc (Procedure id p stmts p') =
    prepend "procedure" $
        prepend id $
        prepend (printPattern p) $
        prepend (printStmts stmts) (printPattern p')
    
printStmt :: Statement -> String
printStmt (Assign op var expr) =
    prepend "assign" $
        prepend (printRevOp op) $
        prepend var (printExpr expr)
printStmt (Loop e1 s1 s2 e2) =
    prepend "loop" $
        prepend (printExpr e1) $
        prepend (printStmts s1) $
        prepend (printStmts s2) (printExpr e2)
printStmt (Conditional e1 s1 s2 e2) =
    prepend "conditional" $
        prepend (printExpr e1) $
        prepend (printStmts s1) $
        prepend (printStmts s2) (printExpr e2)
printStmt (Replacement p1 p2) =
    prepend "replacement" $
        prepend (printPattern p1) (printPattern p2)
printStmt Skip = prepend "skip" "nil"

printExpr :: Expression -> String
printExpr (Constant c) = prepend "constant" (printConst c)
printExpr (EVar v) = prepend "variable" v
printExpr (Operation op e1 e2) =
    prepend "operation" $
        prepend (printOp op) $
        prepend (printExpr e1) (printExpr e2)

printConst :: Constant -> String
printConst (Integer i) = prepend "integer" (show i)
printConst (Atom a) = prepend "atom" a
printConst Nil = prepend "nil" "nil"
printConst (CPair c1 c2) =
    prepend "pair" $
    prepend (printConst c1) (printConst c2)

printPattern :: Pattern -> String
printPattern (PVar v) = prepend "variable" v
printPattern (PPair p1 p2) =
    prepend "pair" $
        prepend (printPattern p1) (printPattern p2)
printPattern (PConst c) = prepend "constant" (printConst c)
printPattern (Involute id p) =
    prepend "involute" $
        prepend id (printPattern p)
printPattern (Call id p) =
    prepend "call" $
        prepend id (printPattern p)
printPattern (Uncall id p) =
    prepend "uncall" $
        prepend id (printPattern p)

printOp :: Op -> String
printOp Xor = "Xor"
printOp Add = "Add"
printOp Sub = "Sub"
printOp Mult = "Mult"
printOp Div = "Div"
printOp Mod = "Mod"
printOp And = "And"
printOp Or = "Or"
printOp Gt = "Gt"
printOp Eq = "Eq"
printOp Lt = "Lt"
printOp GtEq = "GtEq"
printOp LtEq = "LtEq"
printOp Neq = "Neq"

printRevOp :: ReversibleOp -> String
printRevOp XorR = "Xor"
printRevOp AddR = "Add"
printRevOp SubR = "Sub"


list :: [String] -> String
list = foldr prepend "nil"

prepend :: String -> String -> String
prepend x xs = "(" ++ x ++ "." ++ xs ++ ")"