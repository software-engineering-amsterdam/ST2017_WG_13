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

prop_testSet::[Int] -> Bool
prop_testSet xs = (list2set xs) == (list2set xs) 

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = sized (\n -> do
        lst <- choose (0,n) >>= vector
        return $ list2set lst)
    
newtype SetInt = SetInt { unSetInt :: Set Int } deriving (Eq, Show)

instance Arbitrary SetInt where 
    arbitrary = sized (\n -> do
        lst <- vectorOf n (choose (minBound, maxBound))
        return $ (SetInt (list2set lst)))


prop_testSet'::[Int] -> Bool
prop_testSet' xs = (SetInt (Set xs)) == (SetInt (Set xs)) 