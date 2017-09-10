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
