module Primitives where

import Control.Monad.Error

import Value
import Scope
import Error
import Eval

defineSyntax :: LispScope -> [LispVal] -> ThrowsError (LispScope,LispVal)
defineSyntax scope ((Atom lable):values) = do
    value <- evalMap scope values
    return (putValue scope (lable,value) , value)
defineSyntax _ _ = throwError "Define: Syntax error - wrong format"

defineVal = (Syntax defineSyntax)

quoteSyntax :: LispScope -> [LispVal] -> ThrowsError (LispScope,LispVal)
quoteSyntax s [val] = return (s,val)
quoteSyntax _ _ = throwError "Quote: bad syntax"

quoteVal = (Syntax quoteSyntax)

getNum :: LispVal -> ThrowsError Integer
getNum (Number x) = return x 
getNum notNum = throwError "Not a Number"

numFunct :: (Integer->Integer->Integer) -> [LispVal] -> ThrowsError LispVal
numFunct _ [_] = throwError "Too few values for this opperator"
numFunct f vals = do
  xs <- mapM getNum vals
  return $ Number $ foldl1 f xs

plusVal = Function $ numFunct (+)
multVal = Function $ numFunct (*)

topScope = buildScope [
  ("define",defineVal),
  ("quote",quoteVal),
  ("+",plusVal),
  ("*",multVal)]