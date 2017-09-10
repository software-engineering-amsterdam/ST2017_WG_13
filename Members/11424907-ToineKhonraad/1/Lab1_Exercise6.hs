module Lab1_Exercise6 where
  
import Test.QuickCheck

sieve::[Integer] -> [Integer]
sieve (p:xs)  = p: sieve [x | x <- xs, x `mod` p /=0]

primes::[Integer]
primes = sieve [2..]

isPrime :: Integer -> Bool 
isPrime n = all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]


type Runningproduct = Integer

type Counterexamples = [Integer]

type Accumulator = ( Counterexamples, Runningproduct )  


f' :: Accumulator-> Integer -> Accumulator
f' ( counterexamples , runningproduct ) foldValue = 
        
      if isPrime (runningproduct * foldValue + 1 )
      then
          -- not a counterexample 
          ( counterexamples, runningproduct * foldValue )
      else
          -- counterexample => add foldValue to list 
          ( foldValue : counterexamples, runningproduct * foldValue )
  


initialAccumulator = ( [], 1 )

calc_counterexamples = foldl (f') initialAccumulator  (take 10 primes)

{- Time taken: 1.5 hours -}










    
