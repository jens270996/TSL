module TSL.AST where

type Variable = String
type Identifier = String
type Atom = String
data Program = Program Involution [Involution] [Procedure]
    deriving (Eq, Show, Read)


data Involution = Involution Identifier Pattern [Statement]
    deriving (Eq, Show, Read)

data Procedure = Procedure Identifier Pattern [Statement] Pattern
    deriving (Eq, Show, Read)

data Statement =
    Assign ReversibleOp Variable Expression
    |Loop Expression [Statement] [Statement] Expression
    |Conditional Expression [Statement] [Statement] Expression
    | Replacement Pattern Pattern
    | Skip
    deriving (Eq, Show, Read)

data Pattern =
    PVar Variable
    | PPair Pattern Pattern
    | PConst Constant
    | Involute Identifier Pattern
    | Call Identifier Pattern
    | Uncall Identifier Pattern
    deriving (Eq, Read)
instance Show Pattern where
    show (PVar var) = var
    show (PPair p1 p2) = "(" ++ show p1 ++ "." ++ show p2 ++ ")"
    show (PConst c) = "'"++ show c
    show (Involute id p) = "involute " ++ id ++ " " ++ show p
    show (Call id p) = "call " ++ id ++ " " ++ show p
    show (Uncall id p) = "uncall " ++ id ++ " " ++ show p


data Expression =
    Constant Constant
    | EVar Variable
    | Operation Op Expression Expression
    deriving (Eq, Show, Read)

data Constant =
    Integer Int
    | Atom Atom
    | Nil
    | CPair Constant Constant
    deriving (Eq, Read)
instance Show Constant where
    show (Integer i) = show i
    show Nil = "nil"
    show (CPair c1 c2) = "(" ++ show c1 ++ "." ++ show c2 ++ ")"
    show (Atom a) = a


data ReversibleOp =
    XorR
    |AddR
    |SubR
    deriving (Eq, Show, Read)


data Op =
    Xor
    |Add
    |Sub
    |Mult
    |Div
    |Mod
    |And
    |Or
    |Gt
    |Lt
    |Eq
    |Neq
    |GtEq
    |LtEq
    deriving (Eq, Show, Read)

