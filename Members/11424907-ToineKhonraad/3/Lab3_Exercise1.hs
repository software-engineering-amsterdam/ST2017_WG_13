module Lab3_Exercise1 where
  
import Lecture3
import Test.QuickCheck

contradiction :: Form -> Bool
contradiction f = not $ satisfiable f

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails a b = tautology (Impl a b)

equiv :: Form -> Form -> Bool
equiv a b = tautology (Equiv a b )

