module Exercise_3 where
import System.Random
import SetOrd
import Test.QuickCheck
import Data.List
import Exercise_2

{-
    notes for this exercise can be found at:
    https://github.com/software-engineering-amsterdam/ST2017_WG_13/tree/master/Team/4/Exercise_3.md
-}

-- impl
intersect_SetInt :: (Ord a) => Set a -> Set a -> Set a
intersect_SetInt (Set a) (Set b) = list2set (intersect a b)

union_SetInt :: (Ord a) => Set a -> Set a -> Set a
union_SetInt (Set a) (Set b) = list2set (union a b)

difference_SetInt:: (Ord a) => Set a -> Set a -> Set a
difference_SetInt (Set a) (Set b) = list2set ( a \\ b)

-- tests
test_InBothSets          = setIntTester2 1 100 prop_InBothSets
test_InBothSets'         = verboseCheck prop_InBothSets
test_IntersectOrNotSet1  = setIntTester2 1 100 prop_IntersectOrNotSet1
test_IntersectOrNotSet1' = verboseCheck prop_IntersectOrNotSet1
test_IntersectOrNotSet2  = setIntTester2 1 100 prop_IntersectOrNotSet2
test_IntersectOrNotSet2' = verboseCheck prop_IntersectOrNotSet2

test_inAtLeastOneSet = setIntTester2 1 100 prop_inAtLeastOneSet
test_inAtLeastOneSet' = verboseCheck prop_inAtLeastOneSet
test_AllSet1InUnion = setIntTester2 1 100  prop_AllSet1InUnion
test_AllSet1InUnion' = verboseCheck  prop_AllSet1InUnion
test_AllSet2InUnion = setIntTester2 1 100  prop_AllSet2InUnion
test_AllSet2InUnion' = verboseCheck  prop_AllSet2InUnion
test_matchImpl = setIntTester2 1 100 prop_matchedInternalImpl
test_matchImpl' = verboseCheck prop_matchedInternalImpl

test_NotInBothSets = setIntTester2 1 100 prop_NotInBothSets
test_NotInBothSets' = verboseCheck prop_NotInBothSets

testAll = do
  test_InBothSets
  test_IntersectOrNotSet1
  test_IntersectOrNotSet2
  test_inAtLeastOneSet 
  test_AllSet1InUnion
  test_AllSet2InUnion
  test_matchImpl
  test_NotInBothSets

testAllQc = do
  test_InBothSets'
  test_IntersectOrNotSet1'
  test_IntersectOrNotSet2'
  test_inAtLeastOneSet'
  test_AllSet1InUnion'
  test_AllSet2InUnion'
  test_matchImpl'
  test_NotInBothSets'
  
-- properties
prop_InBothSets:: Set Int -> Set Int -> Bool
prop_InBothSets s1 s2 = all (==True) [(inSet x s1) && (inSet x s2) | x <- set2list s3  ]
    where 
        s3 = intersect_SetInt s1 s2

prop_IntersectOrNotSet2:: Set Int -> Set Int -> Bool
prop_IntersectOrNotSet2 s1 s2 = all (==True) [(inSet x s3) || (not $ inSet x s2) | x <- set2list s1  ]
    where 
        s3 = intersect_SetInt s1 s2

prop_IntersectOrNotSet1:: Set Int -> Set Int -> Bool
prop_IntersectOrNotSet1 s1 s2 = all (==True) [(inSet x s3) || (not $ inSet x s1) | x <- set2list s2  ]
    where 
        s3 = intersect_SetInt s1 s2

prop_NotInBothSets:: Set Int -> Set Int -> Bool
prop_NotInBothSets s1 s2 = all (==True) [not $ insetXor x s1 s2 | x <- set2list s3  ]
    where 
        s3 = difference_SetInt s1 s2
        insetXor x s1 s2 = ((inSet x s1) || (inSet x s2)) && (not (inSet x s1) && (inSet x s2))
        
prop_inAtLeastOneSet:: Set Int -> Set Int -> Bool
prop_inAtLeastOneSet s1 s2 = all (==True) [(inSet x s1) || (inSet x s2) | x <- set2list s3  ]
    where 
        s3 = union_SetInt s1 s2

prop_AllSet1InUnion:: Set Int -> Set Int -> Bool
prop_AllSet1InUnion s1 s2 = all (==True) [(inSet x s3) | x <- set2list s1  ]
    where 
        s3 = union_SetInt s1 s2

prop_AllSet2InUnion:: Set Int -> Set Int -> Bool
prop_AllSet2InUnion s1 s2 = all (==True) [(inSet x s3) | x <- set2list s2  ]
    where 
        s3 = union_SetInt s1 s2
        
prop_matchedInternalImpl::Set Int -> Set Int -> Bool
prop_matchedInternalImpl s1 s2 = (unionSet s1 s2) == (union_SetInt s1 s2)

-- from scratch tester
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