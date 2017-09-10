module Lab1_Exercise3 where

import Test.QuickCheck

infix 1 -->
(-->) p q = not p || q



perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
                  insrt x [] = [[x]]
                  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

fact :: Integer -> Integer
fact n | n == 0 = 1
       | otherwise = n * fact(n-1)

f_left :: Integer -> Integer
f_left n = toInteger . length $ (perms [1..n])

{-
  ------------------------------------------------------------------
  Exercise 5 from the workshop

-}

predicate :: Integer -> Bool
predicate n = (n >= 0 && n < 10) --> f_left n == fact n

{-

  This is hard to test, because the left and right side of the 
  equation grow exponentially with n.
-}

{-
  Time taken 1 hour.
  
-}