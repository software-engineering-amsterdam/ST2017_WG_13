module Lab1 where 
import Data.List
import Test.QuickCheck

primes::[Int]
primes = sieve [2..]
sieve::[Int] -> [Int]
sieve (p:xs)  = p: sieve [x | x <- xs, x `mod` p /=0]

isPrime::Int -> Bool
isPrime n = elem n $ takeWhile (<=n) primes

pop::(Int, Int)
pop = pop' 0 
    where
        pop' start 
            | isPrime $ sum slice  = (start, sum slice)
            | otherwise            = pop' (start + 1)  
            where 
                slice = take (101) $ drop start primes

{-
this one was simple, once you see that it is a slicing problem.
done in nearly no time ... had to do some debugging with outputs, but for the most part
one step followed the other and was done in minutes (on the train)
-}