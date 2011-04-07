module SchemeTest where

import Types
import Scope
import Value
import Scheme

import Test.HUnit
import Text.ParserCombinators.Parsec (Parser,parse)

testParse :: (Eq a, Show a)=>String->Parser a->String->a->Test
testParse desc p example expected =
  case (parse p desc example) of
    Right actual ->  TestCase (assertEqual desc expected actual)
    Left err -> TestCase (assertFailure $ show err)

testEval :: String -> String -> LispVal -> Test
testEval desc example expected =
  case (parse parseExpr desc example) of
    Left err -> TestCase (assertFailure $ "Example failed to parse: \n" ++ (show err))
    Right val -> case (eval topLevelScope val) of
        Left  err -> TestCase (assertFailure $ "Example failed to run: \n" ++ (show err))
        Right val -> TestCase (assertEqual desc expected actual)
          where actual = snd (val)

parsingTests = TestList [
    testParse "Integer should parse" parseNumber "42" $ Number 42
  , testParse "Real should parse" parseNumber "42.00" $ Real 42.0

  ,  testParse "Atom should parse" parseAtom "foo" $ Atom "foo"
  , testParse "Atom should pares with symbols" parseAtom "+" $ Atom "+"

  , testParse "List should parse" parseList "(+ 2 2)" $ List [Atom "+", Number 2, Number 2]
  , testParse "List should pares recursivally" parseList "(+ 2 (* 5 7))" $ 
      List [Atom "+", Number 2, List [Atom "*", Number 5, Number 7]]
  , testParse "List should parse with spaces" parseList "(\n  + 2 2\n)   " $ List [Atom "+", Number 2, Number 2]
  , testParse "Quoted List should parse" parseQuoted "'(+ 2 2)" $ 
      List[Atom "quote",List [Atom "+", Number 2, Number 2]]
  ]
  
evalTests = TestList [
    testEval "Bool should be itself" "#t" (Bool True)
  , testEval "Number should be itself" "35" (Number 35) 
  , testEval "String should be itself" "\"Foo\"" (String "Foo")  
  ]

tests = TestList [
    TestLabel "Parsing Tests:" parsingTests
  , TestLabel "Eval Tests:" evalTests
  ]
