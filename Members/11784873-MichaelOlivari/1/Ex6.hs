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

productPlus:: [Integer] -> Integer
productPlus xs = product xs + 1

primeProduct :: [Integer] -> Bool
primeProduct = not . prime . productPlus

smallestCe :: [Integer]
smallestCe = head ([xs | xs <- [takePrimes n | n <- [2 .. ]], primeProduct xs])
