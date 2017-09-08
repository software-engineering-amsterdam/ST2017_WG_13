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

toList :: Integer -> [Integer]
toList n =  if n < 1 then [] else toList (div n 10) ++ [mod n 10]

mutateEvery2nd :: [Integer] -> [Integer]
mutateEvery2nd [] = []
mutateEvery2nd [n] = [n]
mutateEvery2nd (n:k:xs) = n:(if (2*k < 10) then 2*k else (2*k - 9)):mutateEvery2nd xs

luhnTotal :: Integer -> Integer
luhnTotal n = sum (mutateEvery2nd (reverse(toList n)))

isAmericanExpress, isVisa, isMaster :: Integer -> Bool
--https://en.wikipedia.org/wiki/Payment_card_number

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

luhnTest::Integer -> Bool
luhnTest n = (n > 0 && luhn n) --> (not (luhn (n+1)))