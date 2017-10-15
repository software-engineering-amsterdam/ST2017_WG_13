module Exercise_5 where

import Lecture6_Refactored (primeTestsF, prime)
import Exercise_3 (composites)
import Exercise_4 (reportPrimeResult, primesTest)
import Exercise_5_TestData

chernick :: [Integer]
chernick = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

chernickTest runs cnt = primesTest (reportPrimeResult runs) (take cnt chernick)
carmichaelTest runs cnt = primesTest (reportPrimeResult runs) (take cnt firstCarmichaels)
      
slice s l = (take l . drop s)
carmichaelTest' runs cnt s = primesTest (reportPrimeResult runs) (slice s cnt firstCarmichaels)