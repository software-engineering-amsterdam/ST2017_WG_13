module Exercise_4 where
  
import Test.QuickCheck

sieve::[Integer] -> [Integer]
sieve (p:xs)  = p: sieve [x | x <- xs, x `mod` p /=0]

primes::[Integer]
primes = sieve [2..]

isPrime :: Integer -> Bool
isPrime n = all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]

reversal :: Integer -> Integer
reversal = read . reverse. show 

{-
  Just for fun, let's see if reversal tests okay.

-}

predicate :: Integer -> Bool
predicate n = reversal n == reversal ( reversal n)

{-
  Of course: negatives fail the test!
-}

infix 1 -->
(-->) p q = not p || q

predicate' :: Integer -> Bool
predicate' n = (n>=0) --> reversal n == reversal ( reversal n)

{-
  No luck still: numbers divisible by 10 fail!
  
  (This is actually not much of an issue as we will only consider primes)
-}

predicate'' :: Integer -> Bool
predicate'' n = (n>=0 && n `mod` 10 == 0) --> reversal n == reversal ( reversal n)

{-
  For all intends and purposes, we can accept reversal as it is.
  
-}

primesBelow :: Integer -> [ Integer ]
primesBelow n = takeWhile (<=n ) primes

findReversablePrimes :: Integer -> [ Integer ]
findReversablePrimes n = [ x | x <- primesBelow n, isPrime $ reversal x ] 

{-

  Let a final test show that for all 2 < numbers < 10000 they 
  are either:
    0
      in the list
    or
      not prime 
    or
      the reverse is not prime
      
-}

predicate''' x = (2 <= x && x < 10000) --> 
                  x `elem` (findReversablePrimes x) ||
                  not (isPrime x) ||
                  not (isPrime (reversal x))
                  
{-
  Time taken 2.5 hours, due to nasty 'bug' in 'primesBelow' 
  ("<" i.s.o "<=") this failed the last test.  
-}
