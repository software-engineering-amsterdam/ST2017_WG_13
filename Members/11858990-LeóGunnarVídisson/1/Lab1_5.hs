module Lab1_5 where
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
      where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..] 

list1 :: [Integer]

list1 = take(101) primes

isItaPrime = sum list1

isItTrue :: Bool
isItTrue = prime isItaPrime

