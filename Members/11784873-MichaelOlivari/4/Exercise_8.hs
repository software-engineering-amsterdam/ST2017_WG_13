module Exercise_8 where
    
import Data.List
import Test.QuickCheck
import Lecture4
import Exercise_5
import Exercise_6

-- Our valid test relation for the counter model
testRel = [(1,2),(2,3),(3,4)]

x = symClos $ trClos testRel
-- output x = [(1,2),(2,3),(3,4),(1,3),(2,4),(1,4),(2,1),(3,2),(4,3),(3,1),(4,2),(4,1)]
y = trClos $ symClos testRel
-- output y = [(1,2),(2,3),(3,4),(2,1),(3,2),(4,3),(1,3),(1,1),(2,4),(2,2),(3,3),(3,1),(4,4),(4,2),(1,4),(4,1)]

diff = y \\ x
-- output diff = [(1,1),(2,2),(3,3),(4,4)]

{-

It's clear to see that the symmetric closure of the transitive closure
over our test relation contains fewer items than the transitive closure
of the symmetric closure of our test relation. 

This shows that the properties of the relation are not commutative over
each other for all valid relations.

-}


-- A testable property for automatic testing
testCommutative :: Ord a => Rel a -> Bool
testCommutative r = (symClos $ trClos r) == (trClos $ symClos r)

runTest = testCommutative testRel