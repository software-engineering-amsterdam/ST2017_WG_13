module Exercise_6 where

import Data.List
import Test.QuickCheck
import Lecture4
import Exercise_5

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <-s, y == w]

--Total time taken = ~1hr

-- First need a check if the relation is transitive
transCheck :: Ord a => Rel a -> Bool
transCheck r = and [elem (x,z) r | (x,y) <- r, (w,z) <- r, y == w]

-- transitive closure of realtion R is the union of 'n' compositions of R until R is transitive
-- Tr(R) = R U R2 U R3 U .. U R^n
trClos :: Ord a => Rel a -> Rel a
trClos r
    | transCheck r = r
    | otherwise = trClos r' where r' = nub (r ++ (r @@ r))
