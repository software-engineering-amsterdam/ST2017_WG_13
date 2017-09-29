module Exercise_7 where
    
import Data.List
import Test.QuickCheck
import Lecture4
import Exercise_5
import Exercise_6

-- Property checking if a relation is a subrelation of another
isSubset :: Ord a => Rel a -> Rel a -> Bool
isSubset [] _ = True
isSubset ((x,y):xs) ys
        | ((x,y) `elem` ys) = isSubset xs ys
        | otherwise = False


{- Testable Property of Symmetrc Closure

Specification of the symmetric closure:
Given relation "r" and its symmetric closure "s"

-- "r" is subset of "s"
-- "s" is minimal
-- "s" is symmetric i.e. the union of "r" and inverse of "r"
-}
propSymClos :: Ord a => Rel a -> Bool
propSymClos r = isSubset r s && isSym r s && isMinSym r s where s = symClos r

-- test that "r" and inverse of "r" are subsets of "s"
isSym :: Ord a => Rel a -> Rel a -> Bool
isSym [] _ = True
isSym ((x,y):xs) ys 
            | (elem (x,y) ys) && (elem (y,x) ys) = isSym xs ys
            | otherwise = False

-- test that "s" is minimal
isMinSym :: Ord a => Rel a -> Rel a -> Bool
isMinSym r1 r2 = null $ filter (\(a,b) -> (b,a) `notElem` r1) (r2 \\ r1)
            
            

{- Testable Property of Transitive Closure

Specification of the transitive closure:
Given relation "r" and its transitive closure "s"

-- "r" must be a subset of "s"
-- "s" is minimal
-- "s" is the sum of all compositions "r" and is transitive

-}
propTrClos :: Ord a => Rel a -> Bool
propTrClos r = isSubset r s && isMinTran r s && transCheck s where s = trClos r

-- Test for transitivity
-- transCheck function used here from Exercise_6.hs

-- Test that "s" is minimal
isMinTran :: Ord a => Rel a -> Rel a -> Bool
isMinTran r1 r2 = null $ filter (\pair -> transCheck (delete pair r2)) (r2 \\ r1)

-- TODO automated testing using generator from scratch and quickcheck