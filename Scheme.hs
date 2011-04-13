module Scheme where

import Control.Monad
import Control.Monad.Error

import Text.ParserCombinators.Parsec (parse)
import Parser (parseExpr, parseProgram)

import Value
import Eval (evalLisp)
import Primitives (topScope)

replLisp :: LispScope -> String -> IO String
replLisp s input = do
  result <- runErrorT (evalLisp s input)
  return $ either (id) (show) result

evalExpr :: String -> IO String
evalExpr input = do 
  s <- topScope
  replLisp s input
  
buildREPL :: [String] -> IO(String -> IO ())
buildREPL args= do
    s <- topScope
    let quote = "\""
        quoted str = quote ++ str ++ quote
        args' = unwords $ map quoted args
     in do
          replLisp s ("(define args '(" ++ args' ++ "))")
          replLisp s ("(load " ++ args' ++ ")")
    return (\input -> replLisp s input >>= putStrLn)
  
