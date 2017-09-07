module Lab1_Exercise2 where

import Test.QuickCheck
import Data.List

infix 1 -->
(-->) p q = not p || q

powerSet k = subsequences [1..k]


leftV ::  Int -> Int
leftV n = length $ powerSet n

rightV :: Int -> Int
rightV n = 2 ^ n

{-
  ------------------------------------------------------------------
  Exercise 4 from the workshop

-}

predicate n = (n >= 0) --> (leftV n == rightV n)

{-
  The first try of this test 'hangs' and had to be forcefully
  stopped.
  
  As it turns out, 'powerSet k' was BigO( exp k ). Moreover,
  'leftV n' also grows exponentially. So even for small values
  of n the predicate could not be practically determined.
-}

 
predicate' n = (n >= 0 && n < 10) --> (leftV n == rightV n)

{-
  Actually we are testing how subsequences is 'behaving'.
-}

{-
  Time taken 1.5 hours, much due to finding out that subseqeunces 
  grows so fast.
  
-}