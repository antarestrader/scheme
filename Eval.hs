module Eval where

import Control.Monad.Error

import Value
import Scope
import Error
import Parser (readLisp)

-- The eval function takes a scope and paresed Lisp value and
-- returns the resulting Lisp Value.
-- String, Number and Bool return themselves.
-- An Atom trys to find its self in the scope and retrun that value
-- A List is evaluated using the normal Lisp rules.
eval :: LispScope-> LispVal -> IOThrowsError LispVal
eval s val@(String _) = return val
eval s (Atom val) =  do
  (liftIO $ getValue s val) >>= maybe (throwError $ "Value not in scope: " ++ val) (return)
eval s val@(Number _) = return val
eval s val@(Real _) = return val
eval s val@(Bool _) = return val
eval scope (List (fn:lvs)) = do
    fun <- eval scope fn
    case fun of 
      Syntax f _-> f scope lvs
      Function f _-> evalMap scope lvs >>= f
      val -> throwError $ (show val) ++ " is not a function"
eval scope (List []) = return $ List []
eval _ badForm = throwError $ "Unrecognized form: " ++ show badForm

-- Takes a scope an a list of lispVals.  Evaluates each
-- LispVal and returns a list of the results.
evalMap :: LispScope -> [LispVal] -> IOThrowsError [LispVal]
evalMap s = mapM (eval s) 

-- Takes a scope and a list of LispVals.  Evaluates each
-- LispVal in order returning the result of only the final
-- evaluation.
evalLast :: LispScope -> [LispVal] -> IOThrowsError LispVal
evalLast s [] = throwError "No Input Found"
evalLast s [v] = eval s v
evalLast s (v:vs) = eval s v >> evalLast s vs

evalLisp :: LispScope -> String -> IOThrowsError LispVal
evalLisp s input = do
  lvs <- (liftThrows $ readLisp input) 
  (evalLast s lvs)