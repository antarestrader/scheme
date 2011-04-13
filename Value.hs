module Value where

import IO
import Control.Monad
import Control.Monad.Error

import Scope
import Error

type LispScope = Scope LispVal

data Lambda = Lambda {params :: [String], body :: [LispVal]}

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | Real Double
             | String String
             | Function ([LispVal] -> IOThrowsError LispVal) (Either String Lambda)
             | Syntax (LispScope -> [LispVal] -> IOThrowsError LispVal) (Either String Lambda)
             | Bool Bool
             | Port Handle
             
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Real contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Function _ l) = either id showLambda l
showVal (Syntax _ l) = either id showLambda l
showVal (Port _ ) = "<Port>"
showVal (List xs) = "("++ unwords(map showVal xs) ++ ")"

instance Show LispVal where show = showVal

display :: LispVal -> String
display (String contents) = contents
display l = showVal l


showLambda :: Lambda -> String
showLambda Lambda {params = args, body = body} =
  "(lambda ("++ unwords args ++ ") "++ unwords(map showVal body) ++ " )"
  
instance Show Lambda where show = showLambda

analyze :: LispVal -> String
analyze (String contents) = "String(\"" ++ contents ++ "\")"
analyze (Atom name) = "Atom(" ++ name ++ ")"
analyze (Number x) = "Number(" ++ show x ++ ")"
analyze (Real x) = "Real(" ++ show x ++ ")"
analyze (Bool x) = "Bool("++ show x ++ ")"
analyze (List xs) = "List["++ unwords(map analyze xs) ++ "]"
analyze (Function _ _) = "<function>"
analyze (Syntax _ _) = "<syntax>"