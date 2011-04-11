module Scheme where

import Control.Monad
import Control.Monad.Error

import Text.ParserCombinators.Parsec (parse)
import Parser (parseExpr, parseProgram)

import Value
import Scope
import Error
import Eval
import Primitives (topScope)

readLisp :: String -> ThrowsError [LispVal]
readLisp input = case parse parseProgram "(expression)" input of
  Left err -> throwError $ "Parse Error:\n" ++ show err
  Right vals -> return vals

readExpr :: String -> String
readExpr input = case parse parseProgram "(expression)" input of
    Left err -> "No match: " ++ show err
    Right vals ->  unlines (map show vals)

evalLisp :: LispScope -> String -> IOThrowsError LispVal
evalLisp s input = do
  lvs <- (liftThrows $ readLisp input) 
  (evalLast s lvs)

evalExpr :: String -> IO String
evalExpr input = do 
  s <- topScope
  replLisp s input

replLisp :: LispScope -> String -> IO String
replLisp s input = do
  result <- runErrorT (evalLisp s input)
  return $ either (id) (show) result
  
buildREPL :: IO(String -> IO ())
buildREPL = do
  s <- topScope
  return (\input -> replLisp s input >>= putStrLn)

