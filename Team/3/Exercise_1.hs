module Exercise_1 where 
  
import Lecture3
import Test.QuickCheck

{- 
   For notes on this exercise see:
   https://github.com/software-engineering-amsterdam/ST2017_WG_13/blob/master/Team/3/Exercise_1.md
-}

contradiction :: Form -> Bool
contradiction f = not $ satisfiable f

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails a b = tautology (Impl a b)

equiv :: Form -> Form -> Bool
equiv a b = tautology (Equiv a b )