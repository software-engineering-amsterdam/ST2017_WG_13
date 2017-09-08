module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Q3
perms::[a]->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map(insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

prop2::Int -> Bool
prop2 n = length (perms [1..n]) == product [1..n]
