module Scope where

data Scope a = Scope [(String,a)]
    deriving(Show)

buildScope :: [(String,a)] -> Scope a
buildScope input = Scope input

getValue ::  Scope a-> String ->  Maybe a
getValue (Scope s) val = lookup val s

putValue :: Scope a -> (String, a) -> Scope a
putValue (Scope vals) set = Scope (set:vals)
  

