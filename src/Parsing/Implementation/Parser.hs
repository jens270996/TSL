{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parsing.Implementation.Parser where

import TSL.AST
import Text.Parsec
import Utils.Error (ErrorMonad)
import Control.Monad (void)
import Data.Either (lefts, rights)
type Parser = Parsec String ()
type Function = Either Involution Procedure
parseString :: Parser a -> String -> ErrorMonad a
parseString p s = case parse (p <* eof) "" s of
                    Right a -> return a
                    Left e -> Left (show e)


parseProgram:: String -> ErrorMonad Program
parseProgram = parseString (whitespace *> pProgram)
parseInput:: String -> ErrorMonad Constant
parseInput = parseString (whitespace *> constant)

pProgram :: Parser Program
pProgram = do main <- pInvolution
              funcs <- many pFunction
              return $ Program main (lefts funcs) (rights funcs)

pFunction :: Parser (Either Involution Procedure)
pFunction = Left <$> pInvolution <|> Right <$> pProcedure

-- Involutions
pInvolution :: Parser Involution
pInvolution = do keyword "involution"
                 name <- identifier
                 input <- pPattern
                 body <- many1 pStatement
                 return $ Involution name input body

-- Procedures
pProcedure :: Parser Procedure
pProcedure = do keyword "procedure"
                name <- identifier
                input <- pPattern
                body <- manyTill pStatement (keyword "return")
                output <- pPattern
                return $ Procedure name input body output

-- Reversible statement parsers

pStatement :: Parser Statement
pStatement =
    choice [ pLoop
           , pConditional
           , Skip <$ keyword "skip"
           -- must have try since both replacement and assignment can start with variable
           , try pReversibleAssignment
           , pReplacement
           ] <?> "Expecting a statement."
    
pLoop :: Parser Statement
pLoop =
    do keyword "from"
       e1 <- pExpression
       keyword "do"
       s1 <- manyTill pStatement (keyword "loop")
       s2 <- manyTill pStatement (keyword "until")
       e2 <- pExpression
       return $ Loop e1 s1 s2 e2

pConditional :: Parser Statement
pConditional =
    do keyword "if"
       e1 <- pExpression
       keyword "then"
       s1 <- manyTill pStatement (keyword "else")
       s2 <- manyTill pStatement (keyword "fi")
       e2 <- pExpression
       return $ Conditional e1 s1 s2 e2

pReplacement :: Parser Statement
pReplacement =
     do p1 <- pPattern
        symbol "<-"
        p2 <- pPattern
        return $ Replacement p1 p2
    
pReversibleAssignment :: Parser Statement
pReversibleAssignment =
    (do var <- variable
        op <- pReversibleOp
        symbol "="
        exp <- pExpression
        return $ Assign op var exp) <?> "Expecting reversible assignment."

pReversibleOp :: Parser ReversibleOp
pReversibleOp = choice [ AddR <$ symbol "+", SubR <$ symbol "-", XorR <$ symbol "^"]
                    <?> "Expecting reversible operator."


-- Pattern parsers

pPattern :: Parser Pattern
pPattern = choice [ pInvolute
                  , pCall
                  , pUncall
                  , PVar <$> variable
                  , PConst <$> constant
                  , pair pPattern PPair
                  ] <?> "Expecting pattern."
-- Expression parsers


pInvolute :: Parser Pattern
pInvolute = do keyword "involute"
               name <- identifier
               pat <- pPattern
               return $ Involute name pat

pCall :: Parser Pattern
pCall = do keyword "call"
           name <- identifier
           pat <- pPattern
           return $ Call name pat

pUncall :: Parser Pattern
pUncall = do keyword "uncall"
             name <- identifier
             pat <- pPattern
             return $ Uncall name pat

pExpression :: Parser Expression
pExpression = do exp1 <- pExpression1
                 option exp1
                    (do
                        op <- operator0
                        exp2 <- pExpression1
                        return $ Operation op exp1 exp2
                    )

pExpression1 :: Parser Expression
pExpression1 = pExpression2 `chainl1` operator1

pExpression2 :: Parser Expression
pExpression2 = pExpression3 `chainl1` operator2

pExpression3 :: Parser Expression
pExpression3 = pExpression4 `chainl1` operator3

pExpression4 :: Parser Expression
pExpression4 = (Constant <$> constant) <|> (EVar <$> variable) <|> inParentheses pExpression

pOperator :: Parser Op -> Parser (Expression -> Expression -> Expression)
pOperator p = do op <- p 
                 return $ Operation op


operator0 :: Parser Op
operator0 = choice [ Gt <$ symbol ">"
                    , Lt <$ symbol "<"
                    , Eq <$ symbol "="
                    , Neq <$ symbol "!="
                    , GtEq <$ symbol ">="
                    , LtEq <$ symbol "<="]

operator1 :: Parser (Expression -> Expression -> Expression)
operator1 =
    let  p = choice [ And <$ symbol "&&"
                    , Or <$ symbol "||"
                    , Xor <$ symbol "^"]
    in pOperator p

operator2 :: Parser (Expression -> Expression -> Expression)
operator2 =
    let  p = choice [ Add <$ symbol "+"
                    , Sub <$ symbol "-"]
    in pOperator p

operator3 :: Parser (Expression -> Expression -> Expression)
operator3 =
    let  p = choice [ Mult <$ symbol "*"
                    , Div <$ symbol "/"
                    , Mod <$ symbol "%"]
    in pOperator p

constant :: Parser Constant
constant = symbol "'" *> constant'
            where
                constant' =
                    choice [ Integer <$> integer
                           , Nil <$ keyword "nil"
                           , Atom <$> atom
                           , pair constant' CPair
                           ]



-- Auxillary parsers


pair :: Parser el -> (el -> el -> a) -> Parser a
pair p f = (do symbol "("
               el1 <- p
               symbol "."
               el2 <-  p
               symbol ")"
               return $ f el1 el2) <?> "Expecting pair."

atom :: Parser Atom
atom = lexeme $
                do c <- letter
                   cs <- many alphaNum
                   if (c:cs) `elem` keywords then fail "keyword used as atom" else return (c:cs) 


symbol :: String -> Parser ()
symbol s = void . lexeme $ string s

integer :: Parser Int
integer = read <$> lexeme (many1 digit)

keyword :: String -> Parser ()
keyword s = lexeme . try $ string s *> notFollowedBy alphaNum

identifier :: Parser Identifier
identifier = lexeme . try $
                do c <- letter
                   cs <- many alphaNum
                   if (c:cs) `elem` keywords then fail "keyword used as identifier" else return (c:cs)

variable :: Parser Variable
variable =  lexeme. try $
                do c <- letter
                   cs <- many alphaNum
                   if (c:cs) `elem` keywords then fail "keyword used as variable" else return (c:cs)

inParentheses :: Parser a -> Parser a
inParentheses = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

whitespace :: Parser ()
whitespace = many space *> optional (comment >> whitespace)

comment :: Parser ()
comment = void $ try (string "//") *> manyTill anyChar ( void newline <|> eof)




keywords :: [String]
keywords = ["involution","procedure","return","skip","call","uncall","from","do","loop","until","if","then","else","fi","nil"]