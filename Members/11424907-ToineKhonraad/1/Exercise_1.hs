module Exercise_1 where

import Test.QuickCheck

{-
  ------------------------------------------------------------------
  Exercise 2 from the workshop

-}

f_left :: Integer -> Integer
f_left n = sum [ k^2 | k <- [0..n]]

f_right :: Integer -> Integer
f_right n = n * (n + 1) * (2 * n + 1) `div` 6

predicate :: Integer -> Bool
predicate n = f_left n == f_right n

{-
  This first version learned us that k should be a Natural for
  QuickCheck failed on the first try of a negative number!!
  
  This showed how easy it is to oversee the demand for specifying
  the 'contract'.

  After some guidance by Hugo, Michael explained us to do this like
  what follows hereafter:

-}

-- declare the implication operator
infix 1 -->
(-->) p q = not p || q

predicate' :: Integer -> Bool
predicate' n = (n>=0) --> f_left n == f_right n

{-
  ------------------------------------------------------------------
  Exercise 3 from the workshop

-}

f_left' :: Integer -> Integer
f_left' n = sum [ k^3 | k <- [0..n]]

f_right' :: Integer -> Integer
f_right' n = (n * (n + 1) `div` 2) ^ 2

predicate'' :: Integer -> Bool
predicate'' n = (n>=0) --> f_left' n == f_right' n

{-
  Time taken 3+ hours, mostly due to setting up environments, and *lots*
  of (interesting) discussions on how to implement things 'right'.
  
  Refactored many times just to show the solution as accurate and 
  didactically sane as possible.
  
-}
