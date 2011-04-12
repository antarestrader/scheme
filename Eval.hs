module Eval where

import Control.Monad.Error

import Value
import Scope
import Error


eval :: LispScope-> LispVal -> IOThrowsError LispVal
eval s val@(String _) = return val
eval s (Atom val) =  do
  (liftIO $ getValue s val) >>= maybe (throwError $ "Value not in scope: " ++ val) (return)
eval s val@(Number _) = return val
eval s val@(Bool _) = return val
eval scope (List (fn:lvs)) = do
    fun <- eval scope fn
    case fun of 
      Syntax f _-> f scope lvs
      Function f _-> evalMap scope lvs >>= f
      val -> throwError $ (show val) ++ " is not a function"
eval _ badForm = throwError $ "Unrecognized form: " ++ show badForm


evalMap :: LispScope -> [LispVal] -> IOThrowsError [LispVal]
evalMap s = mapM (eval s) 

evalLast :: LispScope -> [LispVal] -> IOThrowsError LispVal
evalLast s [] = throwError "No Input Found"
evalLast s [v] = eval s v
evalLast s (v:vs) = eval s v >> evalLast s vs