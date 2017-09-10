module Lab1 where 
import Data.List
import Test.QuickCheck

primes::[Int]
primes = sieve [2..]
sieve::[Int] -> [Int]
sieve (p:xs)  = p: sieve [x | x <- xs, x `mod` p /=0]

isPrime::Int -> Bool
isPrime n = elem n $ takeWhile (<=n) primes

counterConsPlusOne = counterConsPlusOne' 2
    where
        counterConsPlusOne' plen
            | isConterExample = (primes', total)
            | otherwise       = counterConsPlusOne' (plen + 1)
            where 
                primes' = take (plen) primes
                total = 1 + product primes'
                isConterExample = not $ isPrime total
{-
again pretty simple as I could just reuse the code from the last exercise
-}