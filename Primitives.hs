module Primitives where

import Control.Monad.Error

import Value
import Scope
import Error
import Eval

saveZipOperands :: [String] -> [LispVal] -> ThrowsError [(String,LispVal)]
saveZipOperands p o= 
  if length p == length o
    then 
      return $ zip p o
    else throwError "Function called with the wrong number of operands"

makeFunct :: LispScope -> [String] -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunct scope params body operands = do
  ops <- liftThrows $ saveZipOperands params operands
  s <- liftIO $ deriveScope scope ops
  evalLast s body

extractParam :: LispVal -> ThrowsError String
extractParam (Atom var) = return var
extractParam _ = throwError "Params list must contain only atoms"

defineSyntax :: LispScope -> [LispVal] -> IOThrowsError LispVal
defineSyntax scope ((Atom lable):values) = do
    value <- evalMap scope values >>= return . last
    liftIO $ putValue scope lable value 
    return value
defineSyntax _ _ = throwError "Define: Syntax error - wrong format"

defineVal = (Syntax defineSyntax $ Left "syntax (define)")

lambdaSyntax :: LispScope -> [LispVal] -> IOThrowsError LispVal
lambdaSyntax scope (List params : body) = do
  pars <- liftThrows $ mapM (extractParam) params
  return $ Function (makeFunct scope pars body) $ Right (Lambda pars body)
lambdaSyntax _ _ = throwError "lambda: bad syntax"

lambdaVal = (Syntax lambdaSyntax $ Left "syntax (lambda)")

quoteSyntax :: LispScope -> [LispVal] -> IOThrowsError LispVal
quoteSyntax s [val] = return val
quoteSyntax _ _ = throwError "Quote: bad syntax"

quoteVal = (Syntax quoteSyntax $ Left "syntax (quote)")


getNum :: LispVal -> ThrowsError Integer
getNum (Number x) = return x 
getNum notNum = throwError "Not a Number"


numFunct :: (Integer->Integer->Integer) -> [LispVal] -> IOThrowsError LispVal
numFunct _ [_] = throwError "Too few values for this opperator"
numFunct f vals = do
  xs <- liftThrows $ mapM getNum vals
  return $ Number $ foldl1 f xs

plusVal = Function (numFunct (+)) $ Left "function (+)"
multVal = Function (numFunct (*)) $ Left "function (*)"

topScope = buildScope [
    ("define",defineVal)
    , ("lambda",lambdaVal)
    , ("quote",quoteVal)
    , ("+",plusVal)
    , ("*",multVal)
  ]
