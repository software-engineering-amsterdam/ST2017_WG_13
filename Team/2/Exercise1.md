# Exercise 1

~ Total time spent = stupid amounts (followed lecutures on statistics ... went a bit too deep ... mostly was not needed)
### Testing Randomness of Random Generator ###


#### 1. Specification ####
another developer has given this code and claims it is random

```haskell
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1) 
    return (p:ps)
```
we are to test it by running it a large amount of times and checking if the results are evenly distributed

1) run probs multiple times
2) split the results into buckets
3) check those buckets are relatively evenly split (we choose 0.05% of the total as an acceptable difference)

with a run of 100,000 items we would expect in each bucket 25,000 items +/- 5,000

#### 2. Program ####

1) run multiple times:

```haskell
runXTimes::Int -> Int -> IO [Float]
runXTimes _ 0 = return []
runXTimes n c = liftM2 (++) (probs n) (runXTimes n (c-1))

runLotsOfTimes::Int -> IO [Float]
runLotsOfTimes n = (runXTimes n 10000) 
```

2) split into Buckets

```haskell
fillBuckets xs = foldl addToBucket (0,0,0,0) xs

addToBucket:: (Int,Int,Int,Int) -> Float -> (Int,Int,Int,Int)
addToBucket (a,b,c,d) n
    | n < 0.25  = (a+1,b,c,d)
    | n < 0.5   = (a,b+1,c,d)
    | n < 0.75  = (a,b,c+1,d)
    | otherwise = (a,b,c,d+1)
```

3) decide if it is evenly distributed give or take the delta

```haskell
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
```

4) final program to be tested is pumped into IO monad

```haskell
isRandomSpread :: Int -> IO Bool
isRandomSpread n = (liftM (isRandom . fillBuckets) (runLotsOfTimes n))
```


#### 3. Testing ####

tested using Test.QuickCheck.Monadic to test monadic results - limited to a max of 20 to keep run times
down 

```haskell
prop_isRandomSpread_limited::Property
prop_isRandomSpread_limited = forAll testRange prop_isRandomSpread

testRange:: Gen Int
testRange = choose (1,20)

prop_isRandomSpread:: Int -> Property
prop_isRandomSpread n = monadicIO $ do
    run (isRandomSpread n)
```