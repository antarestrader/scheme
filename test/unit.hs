module SchemeTest where

import Scheme
import Test.HUnit
import Text.ParserCombinators.Parsec (Parser,parse)

testParse :: (Eq a, Show a)=>String->Parser a->String->a->Test
testParse desc p example expected =
  case (parse p desc example) of
    Right actual ->  TestCase (assertEqual desc expected actual)
    Left err -> TestCase (assertFailure $ show err)


test1 = testParse "symbol" symbol "%" '%'

num1 = testParse "Integer" parseNumber "42" $ Number 42
num2 = testParse "Real" parseNumber "42.00" $ Real 42.0

atom1 = testParse "Atom" parseAtom "foo" $ Atom "foo"
atom2 = testParse "Atom" parseAtom "+" $ Atom "+"

list1 = testParse "List" parseList "(+ 2 2)" $ List [Atom "+", Number 2, Number 2]
list2 = testParse "List" parseList "(+ 2 (* 5 7))" $ 
  List [Atom "+", Number 2, List [Atom "*", Number 5, Number 7]]

                         
tests = TestList [test1, num1, num2, atom1, atom2, list1, list2]

