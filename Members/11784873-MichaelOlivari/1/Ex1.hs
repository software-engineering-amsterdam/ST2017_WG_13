module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Q1
func11_1, func11_2::Int -> Int
func11_1 n = sum[(n^2)|n<-[1..n]]
func11_2 n = (n*(n+1)*((2*n)+1)) `div` 6

test1, test1_P::Int -> Bool
test1 n = func11_1 n == func11_2 n -- fails due to negative valuations of n

-- introduce pre-condition to ensure validity of proof only over natural numbers
test1_P n = (n >= 0) --> (func11_1 n == func11_2 n) 

func12_1, func12_2::Int -> Int
func12_1 n = sum[(n^3)|n<-[1..n]] 
func12_2 n = ((n*(n+1)) `div` 2)^2

test2, test2_P::Int -> Bool
test2 n = func12_1 n == func12_2 n -- fails due to negative valuations of n

-- introduce pre-condition to ensure validity of proof only over natural numbers
test2_P n = (n >= 0) --> (func12_1 n == func12_2 n) 

{-

Retrospective:

The main issue when it came to testing these properties was how to handle the precondition,
that n must be a natural number (i.e. >0). I chose to implement this by using the logical
implication operator, which stated that only if the precondition is met will the property hold.
If the precondition is not met, then the test of the property will return true even if the property
itself is false because this means that the validity of the property is dependent on its specification
being met. This ensures that the property is truly tested based on its specification, but limits the
thoroughness of quickCheck's testing. 

Another option is to ensure correct type setting from Int to true natural numbers in order to 
fulfill the precondition, however I feel that utilizing the logical implication property is a more
mathematically sound approach to dealing with specification fulfillment, also because you are then 
dependent on correct implementation of the natural number type setting.

Total Time Spent: 2 hours (getting used to haskell and reasoning about the precondition)

-}