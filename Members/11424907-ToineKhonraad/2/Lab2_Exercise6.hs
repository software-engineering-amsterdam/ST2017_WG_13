module Lab2_Exercise6 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
    
-- Implementing and testing ROT13 encoding

-- ROT13 is a single letter substitution cipher that is used in online forums for hiding spoilers.

-- See also www.rot13.com.

-- First, give a specification of ROT13.

-- Next, give a simple implementation of ROT13.

-- Finally, turn the specification into a series of QuickCheck testable properties, and use these to test 
-- your implementation.

aToz = ['A'..'Z']
leftAToz = take 13 aToz
rightAToz = drop 13 aToz

encLetter :: Char -> Char
encLetter letter 
  | (toUpper letter) `elem` leftAToz = chr (ord letter + 13)
  | (toUpper letter) `elem` rightAToz = chr (ord letter - 13)
  | otherwise = letter

encString :: String -> String
encString s = map encLetter s

p1 s = (encString . encString ) s == s

-- time taken: 15 min. 

  
