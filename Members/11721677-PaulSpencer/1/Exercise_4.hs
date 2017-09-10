module Lab1 where 
import Data.List
import Test.QuickCheck

infix 1 --> 
(-->)::Bool -> Bool -> Bool
p --> q = (not p) || q

-- prime generation inspired from Programming in Haskell, Graham Hutton chapter 15 
primes::[Int]
primes = sieve [2..]
sieve::[Int] -> [Int]
sieve (p:xs)  = p: sieve [x | x <- xs, x `mod` p /=0]

isPrime::Int -> Bool
isPrime n = elem n $ takeWhile (<=n) primes


rint::Int -> Int
rint = read . reverse . show

rps::[Int]
rps = filter (isPrime . rint) $ takeWhile (<10000) primes

-- Tests

propQ4_reverseInlist::Int -> Bool
propQ4_reverseInlist k = elem (rint li) rps 
    where
       li = rps !! k

listIndexes :: Gen Int
listIndexes = choose (0, (length rps) -1)

qcQ4_reversalsInList = quickCheck (forAll listIndexes propQ4_reverseInlist)

propQ4_isPrime::Int -> Bool
propQ4_isPrime k = isPrime $ rps !! k

qcQ4_primeInList = quickCheck (forAll listIndexes propQ4_isPrime)

intDomain:: Gen Int
intDomain = choose (0, 10000)

propQ4_allNonPrimesNotInList::Int -> Bool
propQ4_allNonPrimesNotInList k =  (not $ elem k $ takeWhile (<=10000) primes) --> (not $ elem k rps)

qcQ4_nonPrimesNotInList = quickCheck (forAll intDomain propQ4_allNonPrimesNotInList)

propQ4_primesNotInListRevNotPrime::Int -> Bool
propQ4_primesNotInListRevNotPrime k =  ((elem k $ takeWhile (<=10000) primes) && (not $ elem k rps)) --> (not $ isPrime $ rint k)

qcQ4_nonrevPrimesNotInList = quickCheck (forAll intDomain propQ4_primesNotInListRevNotPrime)

{-
decided to use lots more of the quickCheck functionality
I have no idea how long this took.
-}