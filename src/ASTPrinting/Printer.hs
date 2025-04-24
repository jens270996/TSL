module ASTPrinting.Printer (printProgram,printProgramOrdered) where

import ASTPrinting.Implementation.Printer ( printProgram )
import qualified ASTPrinting.Implementation.PrinterOrdered as PrinterOrdered (printProgram)
import TSL.AST (Program)

printProgramOrdered:: Program -> String
printProgramOrdered = PrinterOrdered.printProgram