module Lab1_4 where
import Test.QuickCheck
{-
  Time:2 Hours
  creating and populating list1 took no time,
  Getting data into list2 took a while.
  Michael help me with that part of the exercise.
-}

reversal :: Integer -> Integer
reversal = read . reverse . show

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
      where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..] 

--This list contins all primes < 10000
list1 :: [Integer]
list1 =  takeWhile(<10000) primes

--List that contains the numbers that are still primes after being reversed

list2 :: [Integer]
list2 = filter (prime . reversal) list1 
