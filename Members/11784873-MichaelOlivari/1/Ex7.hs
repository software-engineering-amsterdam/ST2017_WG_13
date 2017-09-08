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

--Q7
luhn :: Integer -> Bool
luhn n = mod (luhnTotal n) 10 == 0

-- Need a function which converts a integer into a list of its digits
-- inspiration = https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
toList :: Integer -> [Integer]
toList n =  if n < 1 then [] else toList (div n 10) ++ [mod n 10]

-- this is the crucial manipulation, where every other digit starting from the right is doubled in value
-- and the corresponding product sees its individual digits added together. I opted for reducing the 
-- product by 9 due to the simplicity of it and to avoid having to again parse an integer into digits
-- just to add them, reducing the amount of operations / complexity needed to return the correct value.
mutateEvery2nd :: [Integer] -> [Integer]
mutateEvery2nd [] = []
mutateEvery2nd [n] = [n]
mutateEvery2nd (n:k:xs) = n:(if (2*k < 10) then 2*k else (2*k - 9)):mutateEvery2nd xs

-- this is the total that will eventually be checked to see if its value mod 10 == 0. A key thing to
-- note is that the reverse call one the output of the function which parses the numerical string
-- into digits is what ensures that the further call of mutateEvery2nd begins from the right hand
-- side. I had forgotten this condition and this resulted in false negatives during the luhn validation
-- check... I spent much longer than I want to admit trying to fix this.
luhnTotal :: Integer -> Integer
luhnTotal n = sum (mutateEvery2nd (reverse (toList n)))

isAmericanExpress, isVisa, isMaster :: Integer -> Bool

--Source: https://en.wikipedia.org/wiki/Payment_card_number
isAmericanExpress n =
    (luhn n) && 
    (take 2 (toList n) `elem` map toList [34,37]) && 
    (length(toList n) == 15)

isVisa n =
    (luhn n) && 
    (head (toList n) == 4) && 
    (length(toList n) `elem` [13,16,19])

isMaster n =
    (luhn n) && 
    (take 2 (toList n) `elem` map toList [51..55] ||
    take 4 (toList n) `elem` map toList [2221..2720] ) &&
    (length(toList n) == 16)

-- for testing cards https://www.freeformatter.com/credit-card-number-generator-validator.html

-- I'm not sure if this is the best way to perform this test, but the idea is
-- if a numerical string contains only natural numbers (as every credit card does)
-- and the number is valid over the luhn algorithm, then simply changing any
-- single digits value would produce an invalid luhn check on the numbers luhn sum
-- (ie sum ("1234") = 10 mod 10 = 0 while sum ("1334") = 11 mod 10 == 1). 
-- Easiest way to do this was simply adding 1 to the original number.
luhnTest::Integer -> Bool
luhnTest n = (n > 0 && luhn n) --> (not (luhn (n+1)))

{- 

Retrospective:

This was a very fun problem to solve, although it took the longest out of all 
the exercises of Lab1. The biggest problem I encountered was ensuring I was 
getting the correct sum of the mutated numerical string, which kept giving
me false luhn valuations whenever they were supposed to be valid. This was
due to me starting the mutation from the leftmost side of the number rather
than the rightmost side.

Total time spent: ~3 hrs

-}