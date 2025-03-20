module Main where

import Parsing.Parser (parseProgram,parseInput)
import Utils.Error (ErrorMonad)

import Control.Monad
import Options.Applicative
import System.Exit (die)
import Interpretation.Interpreter

data Options = Interpret InterpretOptions

data InterpretOptions = InterpretOptions
  {
    programFile :: String
  , inputFile :: String
  , verbose :: Bool
  }

interpretParser :: Parser Options
interpretParser = Interpret <$> (InterpretOptions
               <$> argument str (metavar "<Program file>")
               <*> argument str (metavar "<Input file>")
               <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Print additional information for debugging purposes.")
              )


optParser :: Parser Options
optParser = hsubparser
              ( command "interpret" (info interpretParser
                (progDesc "Interpret a TSL program"))

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


interpretMain:: InterpretOptions -> IO()
interpretMain InterpretOptions { programFile=programPath, inputFile=inputPath, verbose=v} =
  do program <- parseFile v parseProgram programPath
     trace v $ "Program: \n" ++ show program
     input <- parseFile v parseInput inputPath
     trace v $ "Input: \n" ++ show input
     (case interpretProgram program input of Right c -> print c; Left e -> print $ "Error: " ++ e)


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