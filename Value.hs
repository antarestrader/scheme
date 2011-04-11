module Value where

import Control.Monad
import Control.Monad.Error

import Scope
import Error

type LispScope = Scope LispVal

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | Real Double
             | String String
             | Bool Bool
             | Function ([LispVal] -> IOThrowsError LispVal)
             | Syntax (LispScope -> [LispVal] -> IOThrowsError LispVal)
             
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Real contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Function _) = "<function>"
showVal (Syntax _) = "<syntax>"
showVal (List xs) = "("++ unwords(map showVal xs) ++ ")"

instance Show LispVal where show = showVal


