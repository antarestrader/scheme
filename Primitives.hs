module Primitives where

import Control.Monad.Error

import Value
import Scope
import Error
import Eval
import LispIO (ioPrimitives)

saveZipOperands :: [String] -> [LispVal] -> ThrowsError [(String,LispVal)]
saveZipOperands p o= 
  if length p == length o
    then 
      return $ zip p o
    else throwError "Function called with the wrong number of operands"

makeFunct :: LispScope -> [String] -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunct scope params body operands = do
  ops <- liftThrows $ saveZipOperands params operands
  s <- liftIO $ deriveScope scope ops
  evalLast s body

extractParam :: LispVal -> ThrowsError String
extractParam (Atom var) = return var
extractParam _ = throwError "Params list must contain only atoms"

defineSyntax :: LispScope -> [LispVal] -> IOThrowsError LispVal
defineSyntax scope ((Atom lable):values) = do
    value <- evalLast scope values
    liftIO $ putValue scope lable value 
    return value
defineSyntax _ _ = throwError "Define: Syntax error - wrong format"

defineVal = (Syntax defineSyntax $ Left "syntax (define)")

lambdaSyntax :: LispScope -> [LispVal] -> IOThrowsError LispVal
lambdaSyntax scope (List params : body) = do
  pars <- liftThrows $ mapM (extractParam) params
  return $ Function (makeFunct scope pars body) $ Right (Lambda pars body)
lambdaSyntax _ _ = throwError "lambda: bad syntax"

lambdaVal = (Syntax lambdaSyntax $ Left "syntax (lambda)")

quoteSyntax :: LispScope -> [LispVal] -> IOThrowsError LispVal
quoteSyntax s [val] = return val
quoteSyntax _ _ = throwError "Quote: bad syntax"

quoteVal = (Syntax quoteSyntax $ Left "syntax (quote)")

ifSyntax :: LispScope -> [LispVal] -> IOThrowsError LispVal
ifSyntax s [pred, consequent, alternate] = do
  pred' <- eval s pred
  case pred' of 
    Bool False -> eval s alternate
    otherwise -> eval s consequent
ifSyntax _ _ = throwError "if: Bad Syntax"

ifVal = (Syntax ifSyntax $ Left "syntax (if)")

--Functions----------------------------------

car :: [LispVal] -> IOThrowsError LispVal
car [List(x : xs)] = return x
car _ = throwError "car: takes a single list argument"

cdr :: [LispVal] -> IOThrowsError LispVal
cdr [List(x : xs)] = return $ List xs
cdr _ = throwError "cdr: takes a single non-empty list argument"

cons :: [LispVal] -> IOThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x,  y] = return $ List $ [x,y] -- should be Dotted List nece that gets to be important
cons _ = throwError "cons: this form not implimented"

empty :: [LispVal] -> IOThrowsError LispVal
empty [List []] = return $ Bool True
empty [String ""] = return $ Bool True
empty _ = return $ Bool False

getNum :: LispVal -> ThrowsError Integer
getNum (Number x) = return x 
getNum notNum = throwError "Not a Number"


numFunct :: (Integer->Integer->Integer) -> [LispVal] -> IOThrowsError LispVal
numFunct _ [_] = throwError "Too few values for this opperator"
numFunct f vals = do
  xs <- liftThrows $ mapM getNum vals
  return $ Number $ foldl1 f xs

numBoolFunct :: (Integer -> Integer -> Bool) -> [LispVal] -> IOThrowsError LispVal
numBoolFunct op [Number l, Number r] = return $ Bool $ l `op` r
numBoolFunct _ _ = throwError "Bad Boolan Form"

atomPredFunct :: [LispVal] -> IOThrowsError LispVal
atomPredFunct [(Atom _)] = return $ Bool True
atomPredFunct [_] = return $ Bool False
atomPredFunct _ = throwError "Bad Predicate Form"

stringPredFunct :: [LispVal] -> IOThrowsError LispVal
stringPredFunct [(String _)] = return $ Bool True
stringPredFunct [_] = return $ Bool False
stringPredFunct _ = throwError "Bad Predicate Form"

boolPredFunct :: [LispVal] -> IOThrowsError LispVal
boolPredFunct [(Bool _)] = return $ Bool True
boolPredFunct [_] = return $ Bool False
boolPredFunct _ = throwError "Bad Predicate Form"

numberPredFunct :: [LispVal] -> IOThrowsError LispVal
numberPredFunct [(Number _)] = return $ Bool True
numberPredFunct [(Real _)] = return $ Bool True
numberPredFunct [_] = return $ Bool False
numberPredFunct _ = throwError "Bad Predicate Form"

listPredFunct :: [LispVal] -> IOThrowsError LispVal
listPredFunct [(List _)] = return $ Bool True
listPredFunct [_] = return $ Bool False
listPredFunct _ = throwError "Bad Predicate Form"

plusVal = Function (numFunct (+)) $ Left "function (+)"
minusVal = Function (numFunct (-)) $ Left "function (-)"
multVal = Function (numFunct (*)) $ Left "function (*)"
gtVal = Function (numBoolFunct (>)) $ Left "function (>)"
ltVal = Function (numBoolFunct (<)) $ Left "function (<)"
eqVal = Function (numBoolFunct (==)) $ Left "function (=)"


topScope = buildScope $ [
    ("define",defineVal)
    , ("lambda",lambdaVal)
    , ("quote",quoteVal)
    , ("+",plusVal)
    , ("-",minusVal)
    , ("*",multVal)
    , (">",gtVal)
    , ("<",ltVal)
    , ("=",eqVal)
    , ("_",Bool False)
    , ("if", ifVal)
    , ("atom?", Function atomPredFunct $ Left "function (atom?)")
    , ("number?", Function numberPredFunct $ Left "function (number?)")
    , ("bool?", Function boolPredFunct $ Left "function (bool?)")
    , ("string?", Function stringPredFunct $ Left "function (bool?)")
    , ("list?", Function listPredFunct $ Left "function (list?)")
    , ("car", Function car $ Left "function (car)")
    , ("cdr", Function cdr $ Left "function (cdr)")
    , ("cons", Function cons $ Left "function (cons)")
    , ("empty?", Function empty $ Left "function (empty?)")
  ] ++ ioPrimitives
