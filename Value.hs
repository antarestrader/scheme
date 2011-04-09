module Value where

import Scope

type LispScope = Scope LispVal

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | Real Double
             | String String
             | Bool Bool
             | Function ([LispVal] -> Either String LispVal)
             | Syntax (LispScope -> [LispVal] -> Either String (LispScope,LispVal))
             
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

defineSyntax :: LispScope -> [LispVal] -> Either String (LispScope,LispVal)
defineSyntax scope [Atom lable, value] = Right (putValue scope (lable,value) , value)
defineSyntax _ _ = Left "Define: Syntax error - wrong format"

defineVal = (Syntax defineSyntax)

quoteSyntax :: LispScope -> [LispVal] -> Either String (LispScope,LispVal)
quoteSyntax s [val] = Right (s,val)
quoteSyntax _ _ = Left "Quote: bad syntax"

quoteVal = (Syntax quoteSyntax)

getNum :: LispVal -> Integer
getNum x = case x of 
  Number y -> y
  _ -> 0

numFunct :: (Integer->Integer->Integer) -> ([LispVal] -> Either String LispVal)
numFunct f  = \vals -> Right $ Number (foldl1 f (map getNum vals))

plusVal = Function $ numFunct (+)
multVal = Function $ numFunct (*)

topScope = buildScope [
  ("define",defineVal),
  ("quote",quoteVal),
  ("+",plusVal),
  ("*",multVal)]

