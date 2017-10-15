module Exercise_4

where 

import Data.List
import Control.Monad
import Lecture6_Refactored
import Exercise_3

-- time taken: ~45 mins

falsePrimes :: Int -> [Integer] -> IO [Integer]
falsePrimes _ [] = return []
falsePrimes k (x:xs) = 
            do 
                testFault <- primeTestsF k x
                if testFault then
                    return (x:[])
                else
                    falsePrimes k xs

-- Test function for easy input
test :: Int -> Int -> IO [Integer]
test k n = falsePrimes k (take n composites')

------------------ Testing Results --------------------------

-- Composite '9' fails with k = 1 and 1000 composites
{-
*Exercise_4> test 1 1000
[45]
*Exercise_4> test 1 1000
[9]
*Exercise_4> test 1 1000
[25]
*Exercise_4> test 1 1000
[15]
*Exercise_4> test 1 1000
[237]
*Exercise_4> test 1 1000
[27]
-}

-- composite '25' fails with k = 2
{-
*Exercise_4> test 2 1000
[403]
*Exercise_4> test 2 1000
[451]
*Exercise_4> test 2 1000
[805]
*Exercise_4> test 2 1000
[133]
*Exercise_4> test 2 1000
[427]
*Exercise_4> test 2 1000
[25]
*Exercise_4> test 2 1000
[85]
-}

-- composite '85' fails with k = 3
{-
*Exercise_4> test 3 1000
[1105]
*Exercise_4> test 3 1000
[1105]
*Exercise_4> test 3 1000
[341]
*Exercise_4> test 3 1000
[]
*Exercise_4> test 3 1000
[85]
*Exercise_4> test 3 1000
[1105]
*Exercise_4> test 3 1000
[1105]
-}

-- increasing K increases the accuracy of the primeTestsF, reducing the amount of 
-- composites producing false positives.