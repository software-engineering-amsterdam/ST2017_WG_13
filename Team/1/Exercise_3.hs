module Lab1 where 
import Data.List
import Test.QuickCheck

lhs::Int -> Int
lhs n = length $ permutations [1..n]

factoral::Int -> Int
factoral n = product [1..n] 

rhs::Int -> Int
rhs n = factoral n

-- again exponential growth
propertyHolds::Int -> Bool
propertyHolds k = lhs k == rhs k

-- again can be fixed with preconditions
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

propertyHolds_preCon::Int -> Bool
propertyHolds_preCon k = (0 <= k && k <= 11) --> lhs k == rhs k

-- again with a generator
testableDomain:: Gen Int
testableDomain = choose (0, 11)

-- with generator all tests are valid tests
propertyHoldsForFirst10 = forAll testableDomain propertyHolds

{-
Q - is the property hard to test, why?
A - Yes,

  This is hard to test, because the left and right side of the 
  equation grow exponentially with n.

Q What are you testing?
A We are testing mathematical fact.


The same issues and decisions that were involved in Exercise 2 were also issues here in Exercise 3
we went with the same solution methods.
the solution space is much smaller here because the growth of the factoral
-}