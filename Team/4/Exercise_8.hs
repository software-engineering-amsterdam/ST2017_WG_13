module Exercise_8 where
    
import Data.List
import Test.QuickCheck
import Lecture4
import Exercise_5
import Exercise_6

-- Our valid test relation for the counter model
testRel = [(0,1)]

x = symClos $ trClos testRel
-- output x = [(0,1),(1,0)]
y = trClos $ symClos testRel
-- output y = [(0,1),(1,0),(0,0),(1,1)]

diff = y \\ x
-- output diff = [(0,0),(1,1)]

{-

It's clear to see that the symmetric closure of the transitive closure
over our test relation contains fewer items than the transitive closure
of the symmetric closure of our test relation. 

This shows that the properties of the relation are not commutative over
each other for all valid relations.

-}


-- A testable property for automatic testing
testCommutative :: Rel Int -> Bool
testCommutative r = (symClos $ trClos r) == (trClos $ symClos r)

runTest = testCommutative testRel
