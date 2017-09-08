module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

--Q5
walkBy:: Int->[a]->[[a]] --type which takes an int value specifying the amount of elements to take when walking through the list, in order to generate sublists with each step of the walk
walkBy _ [] = [[]] --stop condition for the tail being an empty list
walkBy n xs = (take n xs) : (walkBy n (tail xs)) --tail-recursion for walking through list, appending each sublist gathered by the walk to main list of subsequences

 --filter on the walks which have a prime sum, taking the head which contains the smallest ints due to walking in the numerically ordered sequence of the initial list
primeSumList = head (filter (prime . sum) (walkBy 101 primes))

--returns smallest sum
minPrimeSum = sum primeSumList

{- This does not need to be tested because the correctness of this evaluation is solely dependent on the inbuilt functions of haskell -}
