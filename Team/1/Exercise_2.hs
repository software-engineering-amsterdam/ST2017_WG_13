module Lab1 where 
import Data.List
import Test.QuickCheck

lhs::Int -> Int
lhs s = length $ subsequences [1..s]

rhs::Int -> Int
rhs s = 2 ^ s

-- on it's own, this will hang on larger inputs because of the exponential growth in output size
-- of subsequences
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

{-
Q - is the property hard to test, why?
A - Yes,

  As it turns out, 'powerSet k' was BigO( exp k ). Moreover,
 'lhs n' also grows exponentially. So even for small values
  of n the predicate could not be practically determined.

Q What are you testing?
A We are testing subsquenses.

This showed that it is not possible to test all things.
Because of limitations on hardware this hangs without limitations on the tests.
We discussed whether using preconditions to limit the property or generators to limit 
quickcheck test domain was correct, however, whatever we  we are in effect limiting 
these tests for non algorythmic restraints.
-}