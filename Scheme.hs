module Scheme where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)

import Value
import Scope


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

parseProgram :: Parser [LispVal]
parseProgram = many parseExpr

eval :: LispScope-> LispVal -> Either String (LispScope,LispVal)
eval s val@(String _) = Right (s,val)
eval s (Atom val) =  case getValue s val of 
    Nothing -> Left $ "Value not in scope: " ++ val
    Just x -> Right (s,x)
eval s val@(Number _) = Right (s,val)
eval s val@(Bool _) = Right (s,val)
eval scope (List (fn:lvs)) = case (eval scope fn) of 
  Right (s,Syntax f) -> f s lvs
  Right (s,Function f) -> apply s f lvs
  Right (_, val) -> Left $ (show val) ++ " is not a function"
  Left err -> Left err

apply :: LispScope ->([LispVal] -> Either String LispVal) -> [LispVal] -> Either String (LispScope,LispVal)
apply scope f vals = case (f vals) of
  Right r -> Right (scope,r)
  Left err -> Left err 

readExpr :: String -> String
readExpr input = case parse parseProgram "(expression)" input of
    Left err -> "No match: " ++ show err
    Right vals ->  unlines (map show vals)

evalExpr :: String -> String
evalExpr input = case parse parseExpr "(expression)" input of
    Left err -> "No match: " ++ show err
    Right val -> case (eval topScope val) of
        Left  err -> "Error: " ++ err
        Right val -> show . snd $ val
