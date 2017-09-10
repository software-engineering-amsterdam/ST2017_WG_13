module Lab1 where 
import Data.List
import Test.QuickCheck

primes::[Integer]
primes = sieve [2..]
sieve::[Integer] -> [Integer]
sieve (p:xs)  = p: sieve [x | x <- xs, x `mod` p /=0]

-- try another prime checker here 
isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\x -> n `rem` x /= 0) ps
    where 
        ps = takeWhile (\y -> y^2 <= n) primes

-- recursivly walk through list of primes checking if the sum of a slice of 101 items is also prime 
primeOf101Primes::Integer
primeOf101Primes = pop 0 101
    where
        pop start len
            | isPrime $ sum slice  = sum slice
            | otherwise            = pop (start + 1) len 
            where 
                slice = take (len) $ drop start primes
