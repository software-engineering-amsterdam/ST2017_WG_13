module Exercise_4

where 

import Data.List
import Control.Monad
import Lecture6_Refactored
import Exercise_3

-- time taken: ~45 mins

falsePrimes :: Int -> [Integer] -> IO Integer
falsePrimes k (x:xs) = 
            do 
                testFault <- primeTestsF k x
                if testFault then
                    return x
                else
                    falsePrimes k xs

test :: Int -> Int -> IO Integer
test k n = falsePrimes k (take n composites')

-- Composite '9' fails with k = 1 and 1000 composites
{-
*Exercise_4> test 1 1000
45
*Exercise_4> test 1 1000
9
*Exercise_4> test 1 1000
25
*Exercise_4> test 1 1000
15
*Exercise_4> test 1 1000
237
*Exercise_4> test 1 1000
27
-}

-- increasing K increases the accuracy of the primeTestsF, reducing the amount of 
-- composites producing false positives.