module Main where

import Parsing.Parser (parseProgram,parseInput)
import Utils.Error (ErrorMonad)

import Control.Monad
import Options.Applicative
import System.Exit (die)
import Interpretation.Interpreter
import ASTPrinting.Printer (printProgram)

data Options = Interpret InterpretOptions | Convert ConvertOptions

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
  , verboseC :: Bool
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


convertMain :: ConvertOptions -> IO ()
convertMain ConvertOptions {sourceFile=inputPath, destinationFile=outputPath, verboseC=v} =
  do program <- parseFile v parseProgram inputPath
     writeFile outputPath (printProgram program)


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
     (case interpretProgram program input of Right c -> putStrLn $ "Output: " ++ show c; Left e -> print $ "Error: " ++ e)




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