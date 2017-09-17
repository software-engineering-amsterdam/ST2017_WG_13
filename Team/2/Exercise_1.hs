module Lab2 where
import Data.List
import Data.Char
import System.Random
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
-- Implement this test, and report on the test results.

-- this is the test:
isRandomSpread :: Int -> IO Bool
isRandomSpread n = (liftM (isRandom . fillBuckets) (runLotsOfTimes n))

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1) 
    return (p:ps)
    
runXTimes::Int -> Int -> IO [Float]
runXTimes _ 0 = return []
runXTimes n c = liftM2 (++) (probs n) (runXTimes n (c-1))

runLotsOfTimes::Int -> IO [Float]
runLotsOfTimes n = (runXTimes n 10000) 

fillBuckets xs = foldl addToBucket (0,0,0,0) xs

addToBucket:: (Int,Int,Int,Int) -> Float -> (Int,Int,Int,Int)
addToBucket (a,b,c,d) n
    | n < 0.25  = (a+1,b,c,d)
    | n < 0.5   = (a,b+1,c,d)
    | n < 0.75  = (a,b,c+1,d)
    | otherwise = (a,b,c,d+1)

eqDelta :: Int -> Int -> Float -> Bool
eqDelta n target delta 
    | (fromIntegral n) > ((fromIntegral target) + delta) = False
    | (fromIntegral n) < ((fromIntegral target) - delta) = False
    | otherwise = True

isRandom (w,x,y,z) = (eqDelta w t d) && (eqDelta x t d) && (eqDelta y t d) && (eqDelta z t d)
    where 
        total = w + x + y + z
        t = total `div` 4
        d = (fromIntegral total) * 0.05
