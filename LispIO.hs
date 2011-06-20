module LispIO where

import IO
import Control.Monad.Error

import Value
import Scope
import Error
import Eval

puts :: [LispVal] -> IOThrowsError LispVal
puts lvs = do
  liftIO $ mapM (putStrLn . display) lvs
  return $ List []
  
load :: LispScope -> [LispVal] -> IOThrowsError LispVal
load _ [] = throwError "No files to load"
load s lvs = do
  lvs' <- evalMap s lvs
  liftM last $ mapM (load' s) lvs'

load' :: LispScope -> LispVal -> IOThrowsError LispVal
load' s (String filename) = (liftIO $ readFile filename) >>= evalLisp s



ioPrimitives = [
    ("puts", Function puts $ Left "IO (puts)")
    , ("load", Syntax load $ Left "IO (load)")
  ]