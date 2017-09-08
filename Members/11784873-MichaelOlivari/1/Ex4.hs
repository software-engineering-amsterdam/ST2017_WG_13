module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Q4
reversal:: Integer -> Integer
reversal = read . reverse . show

factors:: Integer -> [Integer]
factors n = [k | k <- [1..n], n `mod` k == 0] -- returns a complete list of factors of n

prime:: Integer -> Bool
prime n = factors n == [1,n] -- property chekcing if n is prime based on the equivalence of its factor list and the fixed list [1,n]

primes, revPrimes::[Integer]
primes = filter prime [1..10000] --creates the list of primes via filtering on the prime property
revPrimes = filter (prime . reversal) primes  --filter list of primes on the property that the reversal of n is also prime

revProp:: Integer -> Bool
revProp n = ((n > 0) && (n `mod` 10 /= 0)) --> ((reversal $ reversal n) == n) -- n is specified as a natural number and n cannot be divisible by 10 or the reversal would have a leading zero, thus the reversal of the reversal would be the division of n and 10
