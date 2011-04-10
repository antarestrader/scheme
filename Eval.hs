module Eval where

import Control.Monad.Error

import Value
import Scope
import Error


eval :: LispScope-> LispVal -> ThrowsError (LispScope,LispVal)
eval s val@(String _) = return (s,val)
eval s (Atom val) =  case getValue s val of 
    Nothing -> throwError $ "Value not in scope: " ++ val
    Just x -> return (s,x)
eval s val@(Number _) = return (s,val)
eval s val@(Bool _) = return (s,val)
eval scope (List (fn:lvs)) = case (eval scope fn) of 
  Right (s,Syntax f) -> f s lvs
  Right (s,Function f) -> apply s f lvs
  Right (_, val) -> throwError $ (show val) ++ " is not a function"
  Left err -> Left err
eval _ badForm = throwError $ "Unrecognized form: " ++ show badForm

apply :: LispScope ->([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError (LispScope,LispVal)
apply scope f vals = do
    evals <- mapM (eval scope) vals >>= return . map snd
    result <- f evals
    return (scope, result)
    

evalMap :: LispScope -> [LispVal] -> ThrowsError LispVal
--evalMap = undefined
evalMap scope [v] = eval scope v >>= return . snd
evalMap scope (v:vs) = do
  (s,_) <- eval scope v
  evalMap s vs