module Scope where

import Data.IORef

type ScopeImpl a = IORef [(String,IORef a)]

data Scope a = Scope(ScopeImpl a)

nullScope:: IO(Scope a)
nullScope = newIORef([]) >>= return . Scope

buildScope :: [(String,a)] -> IO(Scope a)
buildScope input = do
  s <- nullScope
  mapM (\x -> putValue s (fst x) (snd x)) input
  return s
 
putValue :: Scope a -> String-> a -> IO()
putValue (s@(Scope refenv)) var val = do
  defined <- hasValue s var
  env  <- readIORef refenv
  if (defined)
    then maybe (undefined) (flip writeIORef val) (lookup var env)
    else do
      val' <- newIORef val
      writeIORef refenv ((var,val'):env)

getValue ::  Scope a-> String ->  IO(Maybe a)
getValue (Scope s) val = do
  env <- readIORef s 
  case (lookup val env) of
    Nothing -> return Nothing
    Just ref -> do
      var <- readIORef ref
      return $ Just var
      
hasValue :: Scope a -> String -> IO(Bool)
hasValue (Scope s) val = do
  env <- readIORef s 
  case (lookup val env) of
    Nothing -> return False
    Just _ -> return True
    
dumpScope :: Scope a -> IO[(String,a)]
dumpScope (Scope s) = do
  env <- readIORef s
  mapM dmp env
  
  
dmp :: (String,IORef a) -> IO(String,a)
dmp (var,refval) = do
  val <- readIORef refval
  return (var,val)
    
      
addToScope :: [(String,IORef a)] -> (String, a) -> IO([(String,IORef a)])
addToScope xs (var,val) = do
  val' <- newIORef val
  return ((var,val'):xs)



