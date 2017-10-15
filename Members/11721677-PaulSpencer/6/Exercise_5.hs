module Exercise_5 where

import Lecture6_Refactored (primeTestsF, prime)
import Exercise_3 (composites)
import Exercise_5_TestData

---- primes test (from exercise 4)
reportPrimeResult::Int -> Integer -> IO Int
reportPrimeResult c x = do  
    b <- primeTestsF c x
    if b then putStrLn $ "not prime: " ++ (show x) else putStr ""
    return (if b then 1 else 0)

primesTest::(Integer -> IO Int) -> [Integer] -> IO String
primesTest reporter notPrimes = do
    total <- primesTest' notPrimes
    return ("total: " ++ (show total) ++ " out of: " ++ (show $ length notPrimes))
    where 
        primesTest' = foldr (\x b -> pure (+) <*> (reporter x) <*> b) (return (0)) 
----

chernick :: [Integer]
chernick = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

chernickTest runs cnt = primesTest (reportPrimeResult runs) (take cnt chernick)
carmichaelTest runs cnt = primesTest (reportPrimeResult runs) (take cnt firstCarmichaels)
