module Lab1 where 
import Data.List
import Test.QuickCheck

primes::[Integer]
primes = sieve [2..]
sieve::[Integer] -> [Integer]
sieve (p:xs)  = p: sieve [x | x <- xs, x `mod` p /=0]

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\x -> n `rem` x /= 0) ps
    where 
        ps = takeWhile (\y -> y^2 <= n) primes

-- sligtly modified from exercise 5
counterConsPlusOne = pop 0 2
    where
        pop start len
            | not $ isPrime $ (product slice) +1  = slice
            | otherwise                           = pop start (len +1) 
            where 
                slice = take (len) $ drop start primes

---  or should we go for michaels answer?

-- infinite list of primes growing from 2
primeCombos :: [[Integer]]
primeCombos = [take n primes | n <- [2..]]

notPrimeProduct :: [Integer] -> Bool
notPrimeProduct = not . isPrime . (1+) . product

smallestCounterExample :: [Integer]
smallestCounterExample = head [ps | ps <- primeCombos, notPrimeProduct ps]

solution :: (Integer, [Integer])
solution = ((product smallestCounterExample) + 1, smallestCounterExample)
