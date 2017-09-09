module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Q3
perms::[a]->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map(insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

prop2::Int -> Bool
-- the number of permutations of a list from 1 to n is equivalent to the factorial of n
prop2 n = length (perms [1..n]) == product [1..n]

{-

Question: 1. Was the property difficult to test? 2. What is actually being tested?
Answer: 
1. It is because as n grows, the amount of permutations on the list from 1 to n grows
exponentially, which makes it nearly impossible to check with a standard PC.

2. On first look, prop2 is testing the mathematical theorem that the number of permutations 
of a list from 1 to n is equivalent to the factorial of n. But truthfully we're testing
that perms fulfills its specification in that if the factorial of n is not equivalent to
the number of perms, then the fault lies in perms. This is because product [1..n] is predefined by
Haskell, thus the fault would lie in poor implementation of perms.

Total Time Spent: ~30 minutes

-}