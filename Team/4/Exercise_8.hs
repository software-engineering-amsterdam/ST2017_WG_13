module Exercise_8 where
    
import Data.List
import Test.QuickCheck
import Lecture4
import Exercise_5
import Exercise_6

{-
    notes for this exercise can be found at:
    https://github.com/software-engineering-amsterdam/ST2017_WG_13/tree/master/Team/4/Exercise_8.md
-}

-- Our valid test relation for the counter model
testRel = [(0,1)]

x = symClos $ trClos testRel
-- output x = [(0,1),(1,0)]
y = trClos $ symClos testRel
-- output y = [(0,1),(1,0),(0,0),(1,1)]

diff = y \\ x
-- output diff = [(0,0),(1,1)]


-- A testable property for automatic testing
testCommutative :: Rel Int -> Bool
testCommutative r = (symClos $ trClos r) == (trClos $ symClos r)

runTest = quickCheck testCommutative
