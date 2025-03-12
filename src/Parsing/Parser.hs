module Parsing.Parser where
import TSL.AST
import Utils.Error (ErrorMonad)

parseProgram:: String -> ErrorMonad Program
parseProgram s = Right (Program (Involution "x" (PVar "x") [] SSkip (PVar "x")) [])