module Main where
import System.Environment

main :: IO ()
main = do 
  args <- getArgs
  putStrLn $ concat $ map (\str->"Hello, " ++ str ++ ".\n") args
