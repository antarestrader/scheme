module Scheme where

import Control.Monad
import Control.Monad.Error

import Value
import Scope
import Error
import Eval
import Primitives (topScope)

import Text.ParserCombinators.Parsec (parse)
import Parser (parseExpr, parseProgram)

readLisp :: String -> ThrowsError [LispVal]
readLisp input = case parse parseProgram "(expression)" input of
  Left err -> throwError $ "Parse Error:\n" ++ show err
  Right vals -> return vals

readExpr :: String -> String
readExpr input = case parse parseProgram "(expression)" input of
    Left err -> "No match: " ++ show err
    Right vals ->  unlines (map show vals)

evalLisp :: String -> ThrowsError LispVal
evalLisp input = do
  readLisp input >>= (evalMap topScope)
  

evalExpr :: String -> String
evalExpr input = case parse parseExpr "(expression)" input of
    Left err -> "No match: " ++ show err
    Right val -> case (eval topScope val) of
        Left  err -> "Error: " ++ err
        Right val -> show . snd $ val
