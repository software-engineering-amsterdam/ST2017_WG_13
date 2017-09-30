module Exercise_3 where

  -------------------------------------------------------------------------
  -- Implement operations for set intersection, set union and set 
  -- difference, for the datatype Set defined in SetOrd.hs. 
  -- Next, use automated testing to check that your implementation is 
  -- correct. 
  -- First use your own generator, next use QuickCheck.
  -------------------------------------------------------------------------

  -------------------------------------------------------------------------
  -- Time spent: 
  -------------------------------------------------------------------------
  
  import System.Random
  import SetOrd
  import Test.QuickCheck
  import Data.List
  import Data.Either
  
  import Exercise_2
  
  -------------------------------------------------------------------------
  -- Implementation
  -------------------------------------------------------------------------
  intersect_SetInt :: Set Int -> Set Int -> Set Int
  intersect_SetInt (Set a) (Set b) = list2set (intersect a b)
  
  union_SetInt :: Set Int -> Set Int -> Set Int
  union_SetInt (Set a) (Set b) = list2set (union a b)
  
  difference_SetInt:: Set Int -> Set Int -> Set Int
  difference_SetInt (Set a) (Set b) = list2set ( a \\ b)

  -------------------------------------------------------------------------
  -- Taken from Paul's Exercise_2
  --
  set2list :: Set Int -> [Int]
  set2list (Set ns) = ns
  
  -------------------------------------------------------------------------
  -- Taken from Paul's Exercise_3 (refactored names to match mine
  --
  prop_InBothSets:: Set Int -> Set Int -> Bool
  prop_InBothSets s1 s2 = all (==True) [(inSet x s1) && (inSet x s2) | x <-   set2list s3  ]
    where 
        s3 = intersect_SetInt s1 s2

  prop_NotInBothSets:: Set Int -> Set Int -> Bool
  prop_NotInBothSets s1 s2 = all (==True) [(not $ inSet x s1) || (not $ inSet x s2) | x <- set2list s3  ]
    where 
        s3 = difference_SetInt s1 s2

  prop_inAtLeastOneSet:: Set Int -> Set Int -> Bool
  prop_inAtLeastOneSet s1 s2 = all (==True) [(inSet x s1) || (inSet x s2) | x <- set2list s3  ]
    where 
        s3 = union_SetInt s1 s2

  prop_matchedInternalImpl::Set Int -> Set Int -> Bool
  prop_matchedInternalImpl s1 s2 = (unionSet s1 s2) == (union_SetInt s1 s2)