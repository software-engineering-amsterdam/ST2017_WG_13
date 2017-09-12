module Lab2 where
import Data.List
import Data.Char
import System.Random
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
-- Your programmer Red Curry has written the following function for generating lists of floating point numbers.

-- > probs :: Int -> IO [Float]
-- > probs 0 = return []
-- > probs n = do
-- >              p <- getStdRandom random
-- >              ps <- probs (n-1) 
-- >              return (p:ps)
-- He claims that these numbers are random in the open interval (0..1)(0..1). Your task is to test whether 
-- this claim is correct, by counting the numbers in the quartiles

-- (0..0.25),[0.25..0.5),[0.5..0.75),[0.75..1)(0..0.25),[0.25..0.5),[0.5..0.75),[0.75..1)

-- and checking whether the proportions between these are as expected.

-- E.g., if you generate 10000 numbers, then roughly 2500 of them should be in each quartile.

-- Implement this test, and report on the test results.

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1) 
    return (p:ps)

closedIntervalProperty:: Positive Int -> Property
closedIntervalProperty (Positive n) = monadicIO $ do
    testprobs <- run (probs n)
    assert $ (not $ elem 0 testprobs) && (not $ elem 1 testprobs) 

quartile::Float -> Int
quartile x 
    | x < 0.25  = 0
    | x < 0.5   = 1
    | x < 0.75  = 2
    | otherwise = 3
    
runXTimes::Int -> Int -> IO [Float]
runXTimes _ 0 = return []
runXTimes n c = liftM2 (++) (probs n) (runXTimes n (c-1))

runLotsOfTimes::Int -> IO [Int]
runLotsOfTimes n = liftM (map quartile) (runXTimes n 1000) 


average::[Int] -> Float
average xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))

equalsDelta :: Float -> Float -> Float -> Bool
equalsDelta n target delta 
    | n > (target + delta) = False
    | n < (target - delta) = False
    | otherwise = True

statisticallySignificantProperty:: Positive Int -> Property
statisticallySignificantProperty (Positive n) = monadicIO $ do
    randomQuartiles <- run (runLotsOfTimes n)
    assert $ equalsDelta (average randomQuartiles) 1.5 0.1



probs' :: Int -> IO [Float]
probs' 0 = return []
probs' n = do
    p <- getStdRandom $ randomR (0,0.25)
    q <- getStdRandom $ randomR (0.25,0.5)
    r <- getStdRandom $ randomR (0.5,0.75)
    s <- getStdRandom $ randomR (0.75,1)
    ps <- probs' (n-1)
    return (p:q:r:s:ps)

runXTimes'::Int -> Int -> IO [Float]
runXTimes' _ 0 = return []
runXTimes' n c = liftM2 (++) (probs n) (runXTimes' n (c-1))

runLotsOfTimes::Int -> IO [Int]
runLotsOfTimes n = liftM (map quartile) (runXTimes n 1000) 
    