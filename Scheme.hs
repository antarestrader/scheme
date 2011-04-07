module Scheme where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)

import Scope
import Value
import Types


lexer = P.makeTokenParser haskellDef
number = P.naturalOrFloat lexer
parens = P.parens lexer
lexeme = P.lexeme lexer

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapeSequence :: Parser Char
escapeSequence = do
  char '\\'
  chr <- oneOf "ntr\\\""
  return $ case chr of
    'n' -> '\n'
    't' -> '\t'
    'r' -> '\r'
    '\\'-> '\\' 
    '"'-> chr
   

stringChar :: Parser Char
stringChar = escapeSequence <|> (noneOf "\\\"")

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many stringChar
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber = do
  n <- number
  return $ case n of
    Left val  -> Number val
    Right val -> Real val
    
parseList :: Parser LispVal
parseList = liftM List (parens (many parseExpr))

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseList
         <|> lexeme parseAtom
         <|> lexeme parseString
         <|> parseNumber
         <|> parseQuoted

eval :: Scope-> LispVal -> Either String (Scope,LispVal)
eval s val@(String _) = Right (s,val)
eval s (Atom val) =  case getValue val s of 
    Nothing -> Left $ "Value not in scope: " ++ val
    Just x -> Right (s,x)
eval s val@(Number _) = Right (s,val)
eval s val@(Bool _) = Right (s,val)
eval scope (List (Atom f:lvs)) = Left $ "calling function: " ++ f


readExpr :: String -> String
readExpr input = case parse parseExpr "(expression)" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

evalExpr :: String -> String
evalExpr input = case parse parseExpr "(expression)" input of
    Left err -> "No match: " ++ show err
    Right val -> case (eval topLevelScope val) of
        Left  err -> "Error: " ++ err
        Right val -> show . snd $ val
