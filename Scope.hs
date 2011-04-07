module Scope where

newtype Scope a = Scope [(String,a)]
    deriving(Show)

sDefine :: Scope -> [LispVal] -> Either String (Scope,LispVal)
sDefine scope lvs = case lvs of
    [Atom var, value] -> undefined
    _ -> Left "define: format error"
    
topLevelScope = Scope []

buildScope :: [(String,LispVal)] -> Scope
buildScope input = Scope input

getValue ::  String -> Scope-> Maybe LispVal
getValue val (Scope s) = lookup val s

