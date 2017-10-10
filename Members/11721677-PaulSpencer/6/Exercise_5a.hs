module Exercise_5 where
import Lecture6
import Exercise_1
import System.Random
import Exercise_5_TestData
import Control.Monad

primeTestsF':: Integer -> Integer -> IO Bool
primeTestsF' k n = do
    as <- sequence $ fmap (\_-> randomRIO (2,n-1)) [1..k]
    return (all (\ a -> exM' a (n-1) n == 1) as)

composites':: [Integer]
composites' = [x | x <- [2..], not $ prime x] 


test_fltPrimes:: Integer -> [Integer]  -> IO()
test_fltPrimes runs ts = test_primes primeTestsF' runs ts

test_primes:: (Integer -> Integer -> IO Bool) -> Integer -> [Integer]  -> IO()
test_primes pf runs ts = do 
    t <- gfp runs ts
    let fpct = (*100) $ (fromIntegral t) / (fromIntegral $ length (ts)) in putStrLn $ (show t) ++ " false positives (" ++ (take 6 (show fpct)) ++  "% false)"
        where 
            sc = (sum . (map (\b -> if b then 1 else 0)))
            gfp r = (fmap sc . (sequence . fmap (pf r)))


chernick :: [Integer]
chernick = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

chernickTest runs n = test_fltPrimes  runs (take n chernick)
carmichaelTest runs n = test_fltPrimes runs (take n firstCarmichaels)     

slice s l = (take l . drop s)
carmichaelTest' runs n s = test_fltPrimes runs (slice s n firstCarmichaels)


mrComposite' :: Integer -> Integer -> Bool
mrComposite' x n = let
    (r,s) = decomp (n-1)
    fs     = takeWhile (/= 1) 
       (map (\ j -> exM' x (2^j*s) n)  [0..r])
  in 
    exM' x s n /= 1 && last fs /= (n-1)

primeMR' :: Integer -> Integer -> IO Bool
primeMR' _ 2 = return True
primeMR' 0 _ = return True
primeMR' k n = do 
    a <- randomRIO (2, n-1) :: IO Integer
    if exM' a (n-1) n /= 1 || mrComposite' a n
    then return False else primeMR' (k-1) n

test_MrPrimes:: Integer -> [Integer]  -> IO()
test_MrPrimes runs ts = test_primes primeMR' runs ts

chernickMRTest runs n = test_MrPrimes  runs (take n chernick)
carmichaelMRTest runs n = test_MrPrimes runs (take n firstCarmichaels)     