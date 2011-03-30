module Test where

import Scheme
import Test.HUnit
import Text.ParserCombinators.Parsec (parse)

Right z = (parse symbol "(expression)" "%")

test1 = TestCase (assertEqual "parse symbol"
    z
    '%'
  )
                         
tests = TestList [TestLabel "test1" test1]

