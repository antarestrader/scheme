module Types where

newtype Scope = Scope [(String,LispVal)]
    deriving(Show)

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | Real Double
             | String String
             | Bool Bool
             | Function { scope :: Scope, function :: [LispVal] -> Either String (Scope,LispVal) }
             | Syntax {scope :: Scope, function :: Scope -> [LispVal] -> Either String (Scope,LispVal) }
             
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Real contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Function _) = "<function>"
showVal (Syntax) = "<syntax>"
showVal (List xs) = "("++ unwords(map showVal xs) ++ ")"

instance Show LispVal where show = showVal