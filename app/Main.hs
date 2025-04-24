module Main where

import Parsing.Parser (parseProgram,parseInput, removeLeadingWhitespace)
import Utils.Error (ErrorMonad)

import Control.Monad
import Options.Applicative
import System.Exit (die)
import Interpretation.Interpreter
import ASTPrinting.Printer (printProgram, printProgramOrdered)
import TSL.AST
import Wellformedness.Wellformed

data Options = Interpret InterpretOptions | Convert ConvertOptions | Pair PairOptions

data InterpretOptions = InterpretOptions
  {
    programFile :: String
  , inputFile :: String
  , verbose :: Bool
  }
data ConvertOptions = ConvertOptions
  {
    sourceFile :: String
  , destinationFile :: String
  , ordered :: Bool
  , verboseC :: Bool
  }
data PairOptions = PairOptions
  {
    in1 :: String
  , in2 :: String
  , out :: String
  , verboseP :: Bool
  }


interpretParser :: Parser Options
interpretParser = Interpret <$> (InterpretOptions
               <$> argument str (metavar "<Program file>")
               <*> argument str (metavar "<Input file>")
               <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Print additional information for debugging purposes.")
              )

convertParser :: Parser Options
convertParser = Convert <$> (ConvertOptions
               <$> argument str (metavar "<Program file>")
               <*> argument str (metavar "<Destination file>")
               <*> flag True False (long "ordered"
                           <> short 'o'
                           <> help "If true, use integer encoding of variables.")
               <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Print additional information for debugging purposes.")
              )
pairParser :: Parser Options
pairParser = Pair <$> (PairOptions
               <$> argument str (metavar "<Input file 1>")
               <*> argument str (metavar "<Input file 2>")
               <*> argument str (metavar "<Destination file>")
               <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Print additional information for debugging purposes.")
              )


optParser :: Parser Options
optParser = hsubparser
              ( command "interpret" (info interpretParser
                (progDesc "Interpret a TSL program"))
                <> command "convert" (info convertParser
                    (progDesc "Convert a TSL program into AST used by self-interpreter"))
                <> command "pair" (info pairParser
                    (progDesc "Pair two input files i1 and i2 into a single input (i1.i2)"))
              )

optsParser :: ParserInfo Options
optsParser = info (optParser <**> helper)
  ( fullDesc
  <> progDesc "An interpreter for TSL"
  )



main :: IO ()
main = do
  options <- execParser optsParser
  case options of
    Interpret opts -> interpretMain opts
    Convert opts -> convertMain opts
    Pair opts -> pairMain opts

pairMain :: PairOptions -> IO ()
pairMain PairOptions {in1=path1, in2=path2 , out=pathOut, verboseP=v} =
  do input1 <- readFile path1
     input2 <- readFile path2
     writeFile pathOut ("'(\n" ++ (tail . removeLeadingWhitespace $ input1) ++ "\n.\n" ++ (tail . removeLeadingWhitespace $ input2) ++ "\n)")

convertMain :: ConvertOptions -> IO ()
convertMain ConvertOptions {sourceFile=inputPath, destinationFile=outputPath, verboseC=v, ordered=o} =
  do program <- parseFile v parseProgram inputPath
     writeFile outputPath (if o then (printProgramOrdered program) else (printProgram program))


interpretMain:: InterpretOptions -> IO()
interpretMain InterpretOptions { programFile=programPath, inputFile=inputPath, verbose=v} =
  do trace v "\n" 
     program <- parseFile v parseProgram programPath
     trace v "\n\n\n"
     trace v $ "Program: \n" ++ show program
     trace v "\n"
     input <- parseFile v parseInput inputPath
     trace v "\n"
     trace v $ "Input: " ++ show input
     trace v "\n"
     runProgram program input



runProgram :: Program -> Constant -> IO ()
runProgram program input =
  case wellformedProgram program of
      Nothing -> case interpretProgram program input of
                    Right c -> putStrLn $ "Output: " ++ show c
                    Left e -> putStrLn $ "Error during evaluation: " ++ e
      Just e -> putStrLn $ "Program is not wellformed: " ++ e

parseFile :: Bool -> (String -> ErrorMonad a) -> String ->  IO a
parseFile v parser file =
  do trace v $ "- Reading input from file: " ++ show file
     input <- readFile file
     trace v "- Parsing input"
     fromErrorMonad "parsing" $ parser input


trace :: Bool -> String -> IO()
trace v s = when v $ putStrLn s

fromErrorMonad :: String -> ErrorMonad a -> IO a
fromErrorMonad _ (Right a) = return a
fromErrorMonad s (Left e) = die $ "While " ++ s ++ " Error occurred: " ++ e