module Main where
import System.Environment
import Scheme

main :: IO ()
main = do 
  args <- getArgs
  putStrLn $ readExpr $ args !! 0