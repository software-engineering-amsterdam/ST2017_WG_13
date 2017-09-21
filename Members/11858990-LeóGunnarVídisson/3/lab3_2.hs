module Lab3 where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

--parse :: String -> [Form]
--parse s = [ f | (f,_) <- parseForm (lexer s) ]

--Test for an emty
-- Do a loot of test

--parsTester :: String -> [Form]
testInput :: [String]
testInput =[
  (show form2),
  (show form3),
  (show (Equiv form1 form2)),
  (show (Impl form1 form2)),
  (show (Cnj [form1,form2,form3])),
  (show (Dsj[form1,form2,form3])),
  (show (Impl form2 form3)),
  (show (Equiv form2 form3)),
  (show (Cnj [form1,form2,form3])),
  (show (Dsj[form1,form2,form3])),
  "(2 + 3)",
  "",
  "+(2 3",
  "*(2 3)"]


addBrack :: String -> String
addBrack x = "["++x++"]"

parsTesterHalper :: String -> Bool
parsTesterHalper x = (show((parse x)) == addBrack x)

parsTester :: [String] -> [Bool]
parsTester x = map (parsTesterHalper) x
