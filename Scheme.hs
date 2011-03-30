module Scheme where
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | Real Double
             | String String
             | Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Real contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

instance Show LispVal where show = showVal

lexer = P.makeTokenParser haskellDef
number = P.naturalOrFloat lexer

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

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "(expression)" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

