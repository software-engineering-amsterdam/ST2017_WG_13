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
    -- He claims that these numbers are random in the open interval (0..1). Your task is to test whether 
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
        
    runXTimes::Int -> Int -> IO [Float]
    runXTimes _ 0 = return []
    runXTimes n c = liftM2 (++) (probs n) (runXTimes n (c-1))
    
    runLotsOfTimes::Int -> IO [Float]
    runLotsOfTimes n = (runXTimes n 10000) 
    
    isRandomSpread n = (liftM (isRandom . fillBuckets) (runLotsOfTimes n))
    
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
    