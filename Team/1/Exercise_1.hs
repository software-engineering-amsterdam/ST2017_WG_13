module Lab1 where 
import Data.List
import Test.QuickCheck

-- Workshop 1, Question 2:
--Prove by induction that it holds for all natural numbers n that
--    (1^2)+(2^2)+⋯+(n^2)=(n(n+1)(2n+1))/6.

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

lhsQ2::Integer -> Integer
lhsQ2 k = sum [n^2 | n <- [0..k]]

rhsQ2::Integer -> Integer
rhsQ2 k = (k * (k + 1) * ((2 * k)+1)) `div` 6

propertyHoldsQ2::Integer -> Bool
propertyHoldsQ2 k = (k >= 0) -->lhsQ2 k == rhsQ2 k

-- Workshop 1, Question 3:
-- Prove by induction that it holds for all natural numbers n that
--    (1^3)+(2^3)+⋯+(n^3)=((n(n+1))/2)^2.

lhsQ3::Integer -> Integer
lhsQ3 k = sum [n^3 | n <- [0..k]]

rhsQ3::Integer -> Integer
rhsQ3 k = ((k*(k+1)) `div` 2) ^ 2

propertyHoldsQ3::Integer -> Bool
propertyHoldsQ3 k = (k >= 0) --> lhsQ3 k == rhsQ3 k

{-
Notes:
We had much discussion about whether to use a precondition or to use a Natural Type in order to limit the 
domain of testing.
We were split and ended up choosing a precondition rather than the Natural Type because the set up of the lab
heavily indicates, by showing us the implication definition, that is what we should use.
Please look at Pauls repository for the implementation that uses a type rather than a precondition.
-}