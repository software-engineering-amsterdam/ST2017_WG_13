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
-- returns a complete list of factors of n
factors n = [k | k <- [1..n], n `mod` k == 0]

prime:: Integer -> Bool
 -- property chekcing if n is prime based on the equivalence of its factor list and the fixed list [1,n]
prime n = factors n == [1,n]

primes, revPrimes::[Integer]
primes = filter prime [1..10000]
 --filter list of primes on the property that the reversal of n is also prime
revPrimes = filter (prime . reversal) primes

revProp:: Integer -> Bool
-- n is specified as a natural number and n cannot be divisible by 10 or the reversal would
-- have a leading zero, thus the reversal of the reversal would be the division of n and 10
revProp n = ((n > 0) && (n `mod` 10 /= 0)) --> ((reversal $ reversal n) == n) 

{-

Retrospective:

This was a quick and easy problem that I made more difficult because I didn't realize we 
were given the 'prime' and 'primes' functions, so I made some myself. Mine are probably
more inefficient than the given ones, but it was nice having to think of a way to make 
them myself.

Testing the reversal property was more interesting, as I really didn't know how or where to
start. I found a nice blog showing the prove of the reversal property over a list, which helped
me gain insight into doing it for simple integers.

source: https://www.stackbuilders.com/news/reverse-reverse-theorem-proving-with-idris

Total Time Spent: ~1 hour (mostly the reversal property proof)

-}
