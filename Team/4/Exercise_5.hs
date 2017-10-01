module Exercise_5 where

import Data.List
import Test.QuickCheck
import Lecture4

{-
    notes for this exercise can be found at:
    https://github.com/software-engineering-amsterdam/ST2017_WG_13/tree/master/Team/4/Exercise_5.md
-}

type Rel a = [(a,a)]

inverseR :: Rel a -> Rel a
inverseR [] = []
inverseR ((x,y):xs) = [(y,x)] ++ inverseR xs  

symClos :: Ord a => Rel a -> Rel a
symClos r = nub (r ++ (inverseR r))