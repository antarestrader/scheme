module Value where

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
             
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Real contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Function _ l) = either id showLambda l
showVal (Syntax _ l) = either id showLambda l
showVal (List xs) = "("++ unwords(map showVal xs) ++ ")"

instance Show LispVal where show = showVal

showLambda :: Lambda -> String
showLambda Lambda {params = args, body = body} =
  "(lambda ("++ unwords args ++ ") "++ unwords(map showVal body) ++ " )"


