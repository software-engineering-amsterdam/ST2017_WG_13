module Exercise_5 where

import Data.List
import Test.QuickCheck
import Lecture4

type Rel a = [(a,a)]

{- The symmetric closure of a set R is the union of R and its inverse: R U R'-}
-- Time taken: ~30 mins including reading relation theory
inverseR :: Rel a -> Rel a
inverseR [] = []
inverseR ((x,y):xs) = [(y,x)] ++ inverseR xs  

symClos :: Ord a => Rel a -> Rel a
symClos r = nub (r ++ (inverseR r))