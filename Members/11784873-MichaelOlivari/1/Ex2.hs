module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Q2
func21_1, func21_2::Int -> Int
func21_1 n = 2^(length [1..n])
func21_2 n = length(subsequences [1..n])

prop::Int -> Bool
-- I put the restriction that n must be less than 25 just to ensure the
-- theorem that the two functions are equivalent could be successfully
-- tested. Otherwise quickCheck would be running for much too long do
-- to the exponential growth of the problem.
prop n = (25 >= n && n >= 0) -->  (func21_1 n) == (func21_2 n)


{-

Question: 1. Is the property hard to test? 2. What are you actually testing?

Answer: 
1. It is difficult to test because as n grows, the amount of subsequences on the list from 1 to n grows
exponentially, which makes it nearly impossible to check with a standard PC. By limiting the scope of n, a
successful test of the theorem could be accomplished, however this is not ideal.

2. In this exercise, we're testing subsequences against its specification based on the mathematical fact that
2^n == cardinality of the powerset of a given list with length n.

Total Time Spent = ~25 mins

-}