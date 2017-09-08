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
test1_P n = (n >= 0) --> (func11_1 n == func11_2 n) -- introduce pre-condition to ensure validity of proof only over natural numbers

func12_1, func12_2::Int -> Int
func12_1 n = sum[(n^3)|n<-[1..n]] 
func12_2 n = ((n*(n+1)) `div` 2)^2

test2, test2_P::Int -> Bool
test2 n = func12_1 n == func12_2 n -- fails due to negative valuations of n
test2_P n = (n >= 0) --> (func12_1 n == func12_2 n) -- introduce pre-condition to ensure validity of proof only over natural numbers