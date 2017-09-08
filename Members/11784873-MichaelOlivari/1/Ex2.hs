module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Q2
func21_1, func21_2::Int -> Int
func21_1 n = 2^(length [1..n])
func21_2 n = length(subsequences [1..n])

prop::Int -> Bool
prop n = (25 >= n && n >= 0) -->  (func21_1 n) == (func21_2 n)
