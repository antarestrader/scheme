module Primitives where

import Control.Monad.Error

import Value
import Scope
import Error
import Eval

defineSyntax :: LispScope -> [LispVal] -> IOThrowsError LispVal
defineSyntax scope ((Atom lable):values) = do
    value <- evalMap scope values >>= return . last
    liftIO $ putValue scope lable value 
    return value
defineSyntax _ _ = throwError "Define: Syntax error - wrong format"

defineVal = (Syntax defineSyntax)



quoteSyntax :: LispScope -> [LispVal] -> IOThrowsError LispVal
quoteSyntax s [val] = return val
quoteSyntax _ _ = throwError "Quote: bad syntax"

quoteVal = (Syntax quoteSyntax)


getNum :: LispVal -> ThrowsError Integer
getNum (Number x) = return x 
getNum notNum = throwError "Not a Number"


numFunct :: (Integer->Integer->Integer) -> [LispVal] -> IOThrowsError LispVal
numFunct _ [_] = throwError "Too few values for this opperator"
numFunct f vals = do
  xs <- liftThrows $ mapM getNum vals
  return $ Number $ foldl1 f xs

plusVal = Function $ numFunct (+)
multVal = Function $ numFunct (*)

topScope = buildScope [
    ("define",defineVal)
    , ("quote",quoteVal)
    , ("+",plusVal)
    , ("*",multVal)
  ]
