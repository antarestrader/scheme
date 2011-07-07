module Compile where

import Control.Monad
import Control.Monad.Error

import Value
import Scope
import Error
import Eval

compile :: LispScope -> [LispVal] -> IOThrowsError LispVal
compile s [(List ((Atom "lambda"):(List params):exprs))] = do
  pars <- liftThrows $ mapM (extractParam) params
  
compile s _ = throwError "Cannot compile this expression"

compile' LispScope -> [String] ->

extractParam :: LispVal -> ThrowsError String
extractParam (Atom var) = return var
extractParam _ = throwError "Params list must contain only atoms"