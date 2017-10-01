module Exercise_6 where

import Data.List
import Test.QuickCheck
import Lecture4
import Exercise_5

{-
    notes for this exercise can be found at:
    https://github.com/software-engineering-amsterdam/ST2017_WG_13/tree/master/Team/4/Exercise_6.md
-}

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <-s, y == w]

transCheck :: Ord a => Rel a -> Bool
transCheck r = and [elem (x,z) r | (x,y) <- r, (w,z) <- r, y == w]

trClos :: Ord a => Rel a -> Rel a
trClos r
    | transCheck r = r
    | otherwise = trClos r' where r' = nub (r ++ (r @@ r))