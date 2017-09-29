module Exercise_2 where
 
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Control.Monad

{-
    notes for this exercise can be found at:
    https://github.com/software-engineering-amsterdam/ST2017_WG_13/tree/master/Team/4/Exercise_2.md
-}


-- random by hand:

siGen::Int -> IO (Set Int)
siGen n = do
    l <- getStdRandom (randomR (0,n))
    g <- getStdGen
    return $ list2set $ take l (randoms g :: [Int])

setIntTester :: Int -> Int -> (Set Int -> Set Int) -> (Set Int -> Set Int -> Bool) -> IO ()
setIntTester current total functionToTest isValid = 
    if current == total then 
        print (show total ++ " tests passed")
    else do
        si <- siGen 20
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


-- Quickcheck for Int list input:

prop_setWithList::[Int] -> Bool
prop_setWithList xs = (list2set xs) == (unionEmpty $ list2set xs) 
test_setWithList = verboseCheck prop_setWithList    


-- Quickcheck with Arbitary (Set a)

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = sized (\n -> do
        lst <- arbitraryListGen n
        return $ list2set lst)

arbitraryListGen::(Ord a, Arbitrary a) => Int -> Gen [a]
arbitraryListGen n = choose (0,n) >>= vector

prop_arbitrarySetA::Set Int -> Bool
prop_arbitrarySetA si = si == (unionEmpty si)
test_arbitrarySetA = verboseCheck prop_arbitrarySetA


-- QuickCheck with Arbitary (Set Int) 

newtype SetInt = SetInt { unSetInt :: Set Int } deriving (Eq, Show)

instance Arbitrary SetInt where 
    arbitrary = sized (\n -> do
        lst <- oneof [(intListGen n), (arbitraryListGen n)]
        return $ SetInt $ list2set lst)
    shrink si = map (SetInt . list2set) $ shrink $ set2list $ unSetInt si

intListGen::Int -> Gen [Int]
intListGen n = do
    r <- choose (minBound, maxBound)
    rs <- intListGen (n-1)
    return $ r : (take (n-1) rs)

unionSet'::SetInt -> SetInt -> SetInt 
unionSet' si1 si2 =  SetInt $ unionSet (unSetInt si1) (unSetInt si2)

unionEmpty'::SetInt -> SetInt
unionEmpty' si = unionSet' si (SetInt emptySet)

prop_setInt::SetInt -> Bool
prop_setInt si = si == (unionEmpty' si )
test_setInt = verboseCheck prop_setInt 

-- Testing with shrink

set2list :: Set Int -> [Int]
set2list (Set ns) = ns
    
prop_noNinesInSet::SetInt -> Bool
prop_noNinesInSet si = not $ inSet 9 (unSetInt si)
test_noNinesInSet = verboseCheck prop_noNinesInSet
