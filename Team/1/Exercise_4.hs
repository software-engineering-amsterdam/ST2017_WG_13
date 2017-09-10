module Lab1 where 
import Data.List
import Test.QuickCheck

-- first generate primes:
-- prime generation inspired from Programming in Haskell, Graham Hutton chapter 15 
primes::[Int]
primes = sieve [2..]
sieve::[Int] -> [Int]
sieve (p:xs)  = p: sieve [x | x <- xs, x `mod` p /=0]

-- isPrime inspired from Programming in Haskell, Graham Hutton chapter 5 
factors :: Int -> [Int] 
factors n = [x | x <- [1..n], n `mod` x == 0]
isPrime :: Int -> Bool 
isPrime n = factors n == [1,n]

-- find the reverse of the integer, inspired from lecture notes
reverseint::Int -> Int
reverseint = read . reverse . show

reversablePrimes::[Int]
reversablePrimes = filter (isPrime . reverseint) $ takeWhile (<10000) primes

-- this will fail with out of bounds
reverseInlistInListProperty::Int -> Bool
reverseInlistInListProperty k = elem (reverseint li) reversablePrimes 
    where
       li = reversablePrimes !! k

-- limit the test domain to the valid index positions in the list  
validIndexes :: Gen Int
validIndexes = choose (0, (length reversablePrimes) -1)

reverseInlistInListProperty_withIndex::Property
reverseInlistInListProperty_withIndex = forAll validIndexes reverseInlistInListProperty

-- better double check our differing prime calculations match (one would hope since they are both from Hutton)
allPrimesArePrimesProperty:: NonNegative Int -> Bool
allPrimesArePrimesProperty (NonNegative k) = isPrime $ primes !! k

reversablePrimesArePrimes:: Int -> Bool
reversablePrimesArePrimes k = isPrime $ reversablePrimes !! k

reversablePrimesArePrimes_withIndex::Property
reversablePrimesArePrimes_withIndex = forAll validIndexes reversablePrimesArePrimes

nonReversablePrimes = takeWhile (<10000) primes\\ reversablePrimes

-- limit the test domain to the valid index positions in the nonReversablePrimes list  
validNonPrimeIndexes :: Gen Int
validNonPrimeIndexes = choose (0, (length nonReversablePrimes) -1)

reverseOfPrimesNotInListAreNotPrimes::Int -> Bool
reverseOfPrimesNotInListAreNotPrimes k = (not . isPrime) $ reverseint (nonReversablePrimes !! k) 

reverseOfPrimesNotInListAreNotPrimes_withIndex::Property
reverseOfPrimesNotInListAreNotPrimes_withIndex = forAll validNonPrimeIndexes reverseOfPrimesNotInListAreNotPrimes

{-
Q - how to test?
A - we threw in a load of tests, basically to do the following:

  Let a final test show that for all 2 < numbers < 10000 they 
  are either:
      in the list
    or
      not prime 
    or
      the reverse is not prime


here we decided to take advantage of quickchecks inbuilt ranges for testing

Some tests are probably redundant here.

testing reverseint
-}
