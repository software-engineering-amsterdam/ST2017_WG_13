module Lab2_Exercise4 where
  
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
    
import Lecture2

(-<-) :: Eq a => [a] -> [a] -> Bool
left -<- right = all (True ==) [ x `elem` right  | x <- left  ]

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation left right = left -<- right && right -<- left

--proposed properties weakest to strongest
--   
-- same length
-- all values in left are in right plus above
-- all values in right are in left plus above
-- the values in left and right are in same proportion plus above
-- (if I do not say plus above, then I do not see how one some of these can be subsets, they would be 
-- incompatable i.e.:
--     same length ['a','a','b'] -> ['a','b','a'] ['b','a','a'] ['b','b','a']['a','a','a'] ['b','b','b']['x','y','z'] 
--     all values in left in right ->  ['a','b','a'] ['b','a','a'] ['b','b','a']['a','a','a'] ['a','a','a','b'] 
--     all values in right are left -> ['a','b','a'] ['b','a','a'] ['b','b','a'] ['b','b','b']  ['a','a','a','b'] 
--     values are in same proportion -> ['a','b','a'] ['b','a','a'] ['b','b','a','a','a','a']
-- vs     
--     same length ['a','a','b'] -> ['a','b','a'] ['b','a','a'] ['b','b','a']['a','a','a'] ['b','b','b']['x','y','z'] 
--     all values in left in right ->  ['a','b','a'] ['b','a','a'] ['b','b','a']['a','a','a']  
--     all values in right are left -> ['a','b','a'] ['b','a','a'] ['b','b','a'] 
--     values are in same proportion -> ['a','b','a'] ['b','a','a'] 
-- 



propSelfPermutation :: Eq a => [a] -> Bool
propSelfPermutation a =  isPermutation a a

propReverseInv :: Eq a => [a] -> Bool
propReverseInv a = isPermutation ( reverse a ) a

list1 = [-100,100]
list2 = ['a'..'z']
list3 = []
list4 = [()]

{- Time spent 2 hours -}
