module Lab1 where 
import Data.List
import Test.QuickCheck

lhs::Int -> Int
lhs s = length $ subsequences [1..s]

rhs::Int -> Int
rhs s = 2 ^ s

-- on it's own, this will hang because of the very large size of supersets
propertyHolds::Int -> Bool
propertyHolds s = lhs s == rhs s

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- we found by manually binary searching that 30 was the maximum test input before hanging
-- with precondition it succeeds, but only checks a small number of valid cases
propertyHoldsWithPrecondition::Int -> Bool
propertyHoldsWithPrecondition s = ((0 <= s) && (s <= 30)) --> lhs s == rhs s

testableDomain:: Gen Int
testableDomain = choose (0, 30)

-- with generator all tests are valid tests
propertyHoldsForFirst30 = forAll testableDomain propertyHolds