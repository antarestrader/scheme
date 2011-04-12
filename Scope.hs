module Scope where

import Data.IORef

type ScopeImpl a = IORef [(String,IORef a)]

data Scope a = Scope (ScopeImpl a) (Scope a)
             | Empty

nullScope:: IO(Scope a)
nullScope = do
  ref <- newIORef([]) 
  return $ Scope ref Empty

buildScope :: [(String,a)] -> IO(Scope a)
buildScope input = do
  s <- nullScope
  addToScope s input
  return s
  
deriveScope :: Scope a -> [(String,a)] -> IO(Scope a)
deriveScope p input = do
  ref <- newIORef([])
  s <- return $ Scope ref p
  addToScope s input
  return s
 
putValue :: Scope a -> String-> a -> IO()
putValue (s@(Scope refenv _)) var val = do
  defined <- hasValue s var
  env  <- readIORef refenv
  if (defined)
    then maybe (undefined) (flip writeIORef val) (lookup var env)
    else do
      val' <- newIORef val
      writeIORef refenv ((var,val'):env)

getValue ::  Scope a-> String ->  IO(Maybe a)
getValue Empty _ = return Nothing
getValue (Scope s p) val = do
  env <- readIORef s 
  case (lookup val env) of
    Nothing -> getValue p val
    Just ref -> do
      var <- readIORef ref
      return $ Just var
      
hasValue :: Scope a -> String -> IO(Bool)
hasValue (Scope s p) val = do
  env <- readIORef s 
  case (lookup val env) of
    Nothing -> return False
    Just _ -> return True
    
dumpScope :: Scope a -> IO[(String,a)]
dumpScope (Scope s p) = do
  env <- readIORef s
  mapM dmp env
  
  
dmp :: (String,IORef a) -> IO(String,a)
dmp (var,refval) = do
  val <- readIORef refval
  return (var,val)
    
      
addToScope :: Scope a -> [(String, a)] -> IO()
addToScope s input = do
  mapM (\x -> putValue s (fst x) (snd x)) input
  return ()



