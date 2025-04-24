module ASTPrinting.Implementation.PrinterOrdered where

import TSL.AST
import ASTPrinting.Implementation.Counter
-- '(program . (main . (invols . procs)))
printProgram :: Program -> String
printProgram p =
    let (s,_,_) = runCounter (printProg p) emptyVariables 0
    in s
printProg :: Program -> Counter String
printProg (Program main invols procs) =
    do main' <- printInvol main
       invols' <- printInvols invols
       procs' <- printProcs procs
       return $ "'"++
        (prepend "program" $
        prepend main' $
        prepend invols' procs')

printInvols :: [Involution] -> Counter String
printInvols invols =
    do invols' <- mapM printInvol invols
       return $ list invols'

printProcs :: [Procedure] -> Counter String
printProcs procs =
    do procs' <- mapM printProc procs
       return $ list procs'

printStmts :: [Statement] -> Counter String
printStmts stmts =
    do stmts' <- mapM printStmt stmts
       return $ list stmts'
printInvol :: Involution -> Counter String
printInvol (Involution id p stmts) =
    do p' <- printPattern p
       stmts' <- printStmts stmts
       return $
        prepend "invol" $
        prepend id $
        prepend p' stmts'

printProc :: Procedure -> Counter String
printProc (Procedure id pIn stmts pOut) =
    do pIn' <- printPattern pIn
       pOut' <- printPattern pOut
       stmts' <- printStmts stmts
       return $
        prepend "proc" $
        prepend id $
        prepend pIn' $
        prepend stmts' pOut'

printStmt :: Statement -> Counter String
printStmt (Assign op var expr) =
    do expr' <- printExpr expr
       var' <- getVariable var
       return $
        prepend "assign" $
        prepend (printRevOp op) $
        prepend var' expr'

printStmt (Loop e1 s1 s2 e2) =
    do e1' <- printExpr e1
       s1' <- printStmts s1
       s2' <- printStmts s2
       e2' <- printExpr e2
       return $
        prepend "loopS" $
        prepend e1' $
        prepend s1' $
        prepend s2' e2'
printStmt (Conditional e1 s1 s2 e2) =
    do e1' <- printExpr e1
       s1' <- printStmts s1
       s2' <- printStmts s2
       e2' <- printExpr e2
       return $
        prepend "conditional" $
        prepend e1' $
        prepend s1' $
        prepend s2' e2'
printStmt (Replacement p1 p2) =
    do p1' <- printPattern p1
       p2' <- printPattern p2
       return $
        prepend "replacement" $
        prepend p1' p2'
printStmt Skip = return $ prepend "skipS" "nil"

printExpr :: Expression -> Counter String
printExpr (Constant c) = return $ prepend "constant" (printConst c)
printExpr (EVar v) = getVariableLabel v
printExpr (Operation op e1 e2) =
    do e1' <- printExpr e1
       e2' <- printExpr e2
       return $
        prepend "operation" $
        prepend (printOp op) $
        prepend e1' e2'

printConst :: Constant -> String
printConst (Integer i) = show i
printConst (Atom a) = a
printConst Nil = "nil"
printConst (CPair c1 c2) = prepend (printConst c1) (printConst c2)

printPattern :: Pattern -> Counter String
printPattern (PVar v) = getVariableLabel v
printPattern (PPair p1 p2) =
    do s1 <- printPattern p1
       s2 <- printPattern p2
       return $ prepend "pair" $ prepend s1 s2

printPattern (PConst c) = return $ prepend "constant" (printConst c)
printPattern (Involute id p) =
    do s <- printPattern p
       return $ prepend "involuteS" $
                prepend id s
printPattern (Call id p) =
    do s <- printPattern p
       return $ prepend "callS" $
                prepend id s
printPattern (Uncall id p) =
    do s <- printPattern p
       return $ prepend "uncallS" $
                prepend id s

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

getVariableLabel :: Variable -> Counter String
getVariableLabel v =
    do var <- (getVariable v)
       return $ prepend "variable" var


getVariable :: Variable -> Counter String
getVariable v = do c <- getIdentifier v
                   return (show c)