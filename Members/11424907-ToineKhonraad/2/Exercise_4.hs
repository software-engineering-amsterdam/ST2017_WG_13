module Exercise_4 where
  
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
    
import Lecture2

(-<-) :: Eq a => [a] -> [a] -> Bool
left -<- right = all (True ==) [ x `elem` right  | x <- left  ]

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation left right = left -<- right && right -<- left

propSelfPermutation :: Eq a => [a] -> Bool
propSelfPermutation a =  isPermutation a a

propReverseInv :: Eq a => [a] -> Bool
propReverseInv a = isPermutation ( reverse a ) a

list1 = [-100,100]
list2 = ['a'..'z']
list3 = []
list4 = [()]

{- Time spent 2 hours -}
