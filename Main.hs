module Main where
import System.Environment
import IO hiding (try)
import Scheme

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else action result >> until_ pred prompt action
     
main :: IO ()
main = do 
  args <- getArgs
  repl <- buildREPL args
  until_ (== "exit") (readPrompt "Lisp -> ") (repl)
