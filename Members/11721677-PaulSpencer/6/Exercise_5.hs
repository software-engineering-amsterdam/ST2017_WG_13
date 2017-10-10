module Exercise_5 where
import Exercise_3
import Exercise_4
import Exercise_1
import Exercise_5_TestData
import Lecture6
import System.Random
import Test.QuickCheck
import Control.Monad

chernick :: [Integer]
chernick = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

primeTestsF' :: [Integer] -> Integer -> Bool
primeTestsF' ks n = (all (\ a -> exM' a (n-1) n == 1) ks)

chernickGen,primesGen, carmichaelGen :: Gen (Integer, [Integer]) 
chernickGen = fltGen chernick 30
primesGen = fltGen firstPrimes ((length firstPrimes) -1)
carmichaelGen = fltGen firstCarmichaels ((length firstCarmichaels) -1)

fltGen:: [Integer] -> Int -> Gen (Integer, [Integer]) 
fltGen ns i = do
    i' <- choose (1,i)
    k <- choose (2,10)
    ks <- vectorOf k $ choose (2,10000000)
    let t = (last $ take i' ns) in return (t, map (mod t) ks) -- return ((t, replicate k))

prop_chernicfltCheck = forAll chernickGen (\(fp,ks) -> not $ primeTestsF' ks fp)
prop_carmichaelGen = forAll primesGen (\(fp,ks) -> not $ primeTestsF' ks fp)
prop_primesfltCheck = forAll primesGen (\(fp,ks) -> primeTestsF' ks fp)
t1 = verboseCheck prop_chernicfltCheck
t2 = verboseCheck prop_carmichaelGen
t3 = verboseCheck prop_primesfltCheck
 
