module Exercise_6 where
import Lecture6_Refactored (primeMR, prime)
import Exercise_4 (primesTest, reportPrimeResult)
import Exercise_5 (chernick)
-- import Exercise_5_TestData

reportMRPrimeResult::Int -> Integer -> IO Int
reportMRPrimeResult c x = do  
    b <- primeMR c x
    if b then putStrLn $ "not prime: " ++ (show x) else putStr ""
    return (if b then 1 else 0)

chernickMRTest runs cnt = primesTest (reportMRPrimeResult runs) (take cnt chernick)

---------------------------------

printMer::Integer -> IO Integer
printMer x = do  
    b <- primeMR 10 ((2^x)-1)
    if b then putStrLn $ (show x) ++ " = (2^" ++ (show x) ++ "-1)" else putStr ""
    return (if b then 1 else 0)

test_mersprimes::[Integer] -> IO String
test_mersprimes ts =  return ("total " ++) <*> (return show <*> (mp ts))
    where 
        mp = foldr (\x b -> pure (+) <*> (printMer x) <*> b) (return (0))    