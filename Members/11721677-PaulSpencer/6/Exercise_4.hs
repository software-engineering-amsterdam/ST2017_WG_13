module Exercise_4

where 

import Data.List
import Control.Monad
import Lecture6
import Exercise_3


-- falsePrimes :: Int -> [Integer] -> IO [Integer]
-- falsePrimes _ [] = return []
-- falsePrimes k (x:xs) = 
--             do 
--                 testFault <- primeTestsF k x
--                 if testFault then
--                     do
--                         --print ("False postive composite: " ++ show x)
--                         --next <- falsePrimes k xs
--                         return ([x])
--                 else
--                     falsePrimes k xs



falsePrimes :: Int -> [Integer] -> IO [Integer]
falsePrimes _ [] = return []
falsePrimes k (x:xs) = do 
    falsePrime <- primeTestsF k x
    if falsePrime then
        do
            next <- falsePrimes k xs
            return (x:next)
    else
        falsePrimes k xs

mycomps = filter (not . prime) [3..562]

test :: Int -> IO [Integer]
test k = falsePrimes k mycomps

ptest k r = liftM ((/r) . fromIntegral . length . concat) $ ptest' r k
    where 
        ptest' 0 _ = return []
        ptest' z k = do
            v <- test k 
            vs <- ptest' (z-1) k
            return (v:vs)
