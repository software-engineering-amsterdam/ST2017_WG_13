module Exercise_4 where
import Lecture6_Refactored (primeTestsF)
import Exercise_3 (composites)
import Control.Monad (replicateM)

-- single runs

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

fltTest :: Int -> Int -> IO String
fltTest runs cnt = primesTest (reportPrimeResult runs) (take cnt composites)

-- run multiple times
primesTestMin::Int -> [Integer] -> IO (Maybe Integer)
primesTestMin runs notPrimes = do
    findMin notPrimes
    where 
        findMin = foldr (\y m -> pure (maybeMin) <*> (prm y) <*> m) (return (Nothing)) 
        prm x = primeTestsF runs x >>= \b -> return (if b then pure x else Nothing)

maybeMin::Maybe Integer -> Maybe Integer -> Maybe Integer
maybeMin (Just x2) (Just y2) = Just (min x2 y2)
maybeMin x2 y2 = if x2 == Nothing then y2 else x2     

maybeMinList::[Maybe Integer] -> Maybe Integer
maybeMinList = foldr (\x b -> maybeMin x b) (Nothing)

fltTestMin :: Int -> Int -> IO (Maybe Integer)
fltTestMin runs cnt = primesTestMin runs (take cnt composites)
    
multiRun:: Int -> IO (Maybe Integer) -> IO (Maybe Integer)
multiRun n f = pure (maybeMinList) <*> (replicateM n f)

-- fermats little theorum that don't catch false positives after multiple attempts
flt1, flt2, flt3, flt4, flt5, flt6, flt7, flt8, flt9, flt10, flt11, flt12, flt20:: IO (Maybe Integer) 
flt1 = multiRun 1000 (fltTestMin 1 100)    -- 1000 tests (fp = 9)
flt2 = multiRun 1000 (fltTestMin 2 300)    -- 2000 tests (fp = 9)
flt3 = multiRun 1000 (fltTestMin 3 300)    -- 3000 tests (fp = 9)
flt4 = multiRun 1000 (fltTestMin 4 300)    -- 4000 tests (fp = 9)
flt5 = multiRun 1500 (fltTestMin 5 300)    -- 7500 tests (fp = 15)
flt6 = multiRun 2000 (fltTestMin 6 300)    -- 12000 tests (fp = 15)
flt7 = multiRun 3000 (fltTestMin 7 300)    -- 21000 tests (fp = 91)
flt8 = multiRun 5000 (fltTestMin 8 600)    -- 40000 tests (fp = 91)
flt9 = multiRun 8000 (fltTestMin 9 600)    -- 72000 tests (fp = 91)
flt10 = multiRun 8000 (fltTestMin 10 600)  -- 80000 tests (fp = 91)
flt11 = multiRun 10000 (fltTestMin 11 600) -- 110000 tests (fp = 561)
flt12 = multiRun 8000 (fltTestMin 12 600)  -- 96000 tests (fp = 561)
flt20 = multiRun 20000 (fltTestMin 20 600)  -- ran out of memory with 4000000 tests (with 160000 tests nothing)
