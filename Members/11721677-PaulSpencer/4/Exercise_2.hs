module Exercise_2 where
 
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Control.Monad

siSizedGen::Int -> IO (Set Int)
siSizedGen n = do
    g <- getStdGen
    l <- getStdRandom (randomR (0,n))
    return $ list2set $ take l (randoms g :: [Int])
    
setIntTester :: Int -> Int -> (Set Int -> Set Int) -> (Set Int -> Set Int -> Bool) -> IO ()
setIntTester current total functionToTest isValid = 
    if current == total then 
        print (show total ++ " tests passed")
    else do
        si <- siSizedGen 20
        if isValid si (functionToTest si) then
            do 
                print ("pass on: " ++ show si)
                setIntTester (current+1) total functionToTest isValid
        else 
            error ("failed test on: " ++ show si)

prop_areEqual::Set Int -> Set Int -> Bool
prop_areEqual x y = x == y

unionEmpty::Set Int -> Set Int
unionEmpty si = unionSet si emptySet

test_areEqual::IO()
test_areEqual = setIntTester 1 100 unionEmpty prop_areEqual


prop_testSetWithList::[Int] -> Bool
prop_testSetWithList xs = (list2set xs) == (list2set xs) 
    
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = sized (\n -> do
        lst <- choose (0,n) >>= vector
        return $ list2set lst)

prop_testSet::Set Int -> Bool
prop_testSet si = si == si

newtype SetInt = SetInt { unSetInt :: Set Int } deriving (Eq, Show)

instance Arbitrary SetInt where 
    arbitrary = sized (\n -> do
        lst <- oneof [(intListGen n),choose (0,n) >>= vector]
        return $ SetInt $ list2set lst)
    shrink si = map (SetInt . list2set) $ shrink $ set2list $ unSetInt si

intListGen::Int -> Gen [Int]
intListGen n = do
    r <- choose (minBound, maxBound)
    rs <- intListGen (n-1)
    return $ r : (take n rs)

set2list :: Set Int -> [Int]
set2list (Set ns) = ns
    

prop_testSet'::SetInt -> Bool
prop_testSet' si = si == si

prop_testSetNoNine::SetInt -> Bool
prop_testSetNoNine si = not $ inSet 9 (unSetInt si)