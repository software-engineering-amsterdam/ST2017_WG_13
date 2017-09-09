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

--Q6
takePrimes :: Int -> [Integer]
takePrimes n = take n primes

factorialPlus1:: [Integer] -> Integer
factorialPlus1 xs = product xs + 1

-- simple function which checks that p1 * .. * pn is not prime 
primeProduct :: [Integer] -> Bool
primeProduct = not . prime . factorialPlus1

-- xs is stated to be a subseqeunce of the list of primes, with the property that
-- their "product" (the factorial of elements plus 1) is not prime. This generates
-- a list of counter examples, of which we take the head element as it contains
-- the list of minimum primes which fulfill the primeProduct property. 
smallestCE :: [Integer]
smallestCE = head [xs | xs <- [takePrimes n | n <- [2 .. ]], primeProduct xs]

solution :: [[Integer]]
solution = (factorialPlus1(smallestCE) : []) : (smallestCE : [])

{-

Retrospective:

This exercise proved to be the most difficult for me just due to inexperience with
Haskell and nested list comprehensions. I'm sure there is an easier way of doing 
this but it's the best I could do so far.

Total Time Spent: ~2 hours

-}