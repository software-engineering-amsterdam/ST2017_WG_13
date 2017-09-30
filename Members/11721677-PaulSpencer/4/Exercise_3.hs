module Exercise_3 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise_2
import Debug.Trace
{-
    notes for this exercise can be found at:
    https://github.com/software-engineering-amsterdam/ST2017_WG_13/tree/master/Team/4/Exercise_2.md
-}

{-
    Implement operations for set intersection, set union and set difference, for the datatype Set 
    defined in SetOrd.hs. Next, use automated testing to check that your implementation is correct. 
    First use your own generator, next use QuickCheck.

    (Deliverables: implementations, test properties, short test report, indication of time spent.)
-}

-- implement intersect
intersectSet :: (Ord a) => Set a -> Set a -> Set a 
intersectSet _ (Set []) =  emptySet
intersectSet (Set []) _ =  emptySet
intersectSet s1@(Set (x:xs)) s2 
    | s1 == s2 = s1
    | inSet x s2 = insertSet x (intersectSet (Set xs) s2)
    | otherwise = intersectSet (Set xs) s2

-- implement diff

diffSet :: (Ord a) => Set a -> Set a -> Set a 
diffSet s1 (Set []) =  s1
diffSet (Set []) s2 =  s2
diffSet s1@(Set (x:xs)) s2 
    | s1 == s2 = emptySet
    | inSet x s2 = diffSet (Set xs) (deleteSet x s2)
    | otherwise = insertSet x (diffSet (Set xs) s2)

-- implement union (already done, so lets try a different way)

unionSet2:: (Ord a) => Set a -> Set a -> Set a 
unionSet2 (Set xs) (Set ys) = list2set (xs ++ ys)

-- test intesect with random
setIntTester2 :: Int -> Int -> (Set Int -> Set Int -> Bool) -> IO ()
setIntTester2 current total property = 
    if current == total then 
        print (show total ++ " tests passed")
    else do
        si1 <- siGen 20        
        si2 <- siGen 20
        if property si1 si2 then
            do 
                print ("pass on: " ++ show si1++ " and  " ++ show si2)
                setIntTester2 (current+1) total property
        else 
            error ("failed test on: " ++ show si1++ " and  " ++ show si2)

prop_InBothSets:: Set Int -> Set Int -> Bool
prop_InBothSets s1 s2 = all (==True) [(inSet x s1) && (inSet x s2) | x <- set2list s3  ]
    where 
        s3 = intersectSet s1 s2

prop_NotInBothSets:: Set Int -> Set Int -> Bool
prop_NotInBothSets s1 s2 = all (==True) [(not $ inSet x s1) || (not $ inSet x s2) | x <- set2list s3  ]
    where 
        s3 = diffSet s1 s2

prop_inAtLeastOneSet:: Set Int -> Set Int -> Bool
prop_inAtLeastOneSet s1 s2 = all (==True) [(inSet x s1) || (inSet x s2) | x <- set2list s3  ]
    where 
        s3 = unionSet2 s1 s2

prop_matchedInternalImpl::Set Int -> Set Int -> Bool
prop_matchedInternalImpl s1 s2 = (unionSet s1 s2) == (unionSet2 s1 s2)

--- test with from scrath

test_InBothSets = setIntTester2 1 100 prop_InBothSets
test_NotInBothSets = setIntTester2 1 100 prop_NotInBothSets
test_inAtLeastOneSet = setIntTester2 1 100 prop_inAtLeastOneSet
test_matchImpl = setIntTester2 1 100 prop_matchedInternalImpl

-- test intersect with QuickCheck

test_InBothSets' = verboseCheck prop_InBothSets
test_NotInBothSets' = verboseCheck prop_NotInBothSets
test_inAtLeastOneSet' = verboseCheck prop_inAtLeastOneSet
test_matchImpl' = verboseCheck prop_matchedInternalImpl
