module Exercise_4

where 

import Data.List
import Control.Monad
import Lecture6
import Exercise_3


falsePrimes :: Int -> [Integer] -> IO [Integer]
falsePrimes _ [] = return []
falsePrimes k (x:xs) = 
            do 
                testFault <- primeTestsF k x
                if testFault then
                    do
                        print ("False postive composite: " ++ show x)
                        next <- falsePrimes k xs
                        return (x:next)
                else
                    falsePrimes k xs


{-
falsePrimes :: Int -> [Integer] -> [Integer]
falsePrimes _ [] = return []
falsePrimes k (x:xs)
                | primeTestsF k x = x : falsePrimes k xs
                | otherwise = falsePrimes k xs
-}

test :: Int -> Int -> IO [Integer]
test k n = falsePrimes k (take n composites')

-- Composite '9' fails with k = 1 and 1000 composites
-- test 1 1000 : [9,35,66,154,155,221,255,259,265,301,366,429,451,545,561,703,745,781,805,1045,1057,1105]