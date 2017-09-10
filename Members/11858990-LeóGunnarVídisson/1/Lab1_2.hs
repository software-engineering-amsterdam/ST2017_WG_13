module Lab1_2 where
import Test.QuickCheck
import Data.List
--Part 2
{-
  Time: 3 Hours
  Since we got 2^a It grows exponentially
  There for the time it takes to calculate big datasets 
  is heading to inf
-}

infix 1 -->
(-->)p q = not p || q

list1 :: Int -> [Int]
list1 k = [1..k]

powerset :: [Int] -> [[Int]]
powerset list1 = subsequences list1

checker1 :: Int -> Int
checker1 k = length (powerset $ list1 k)

checker2 a = 2 ^ a
test x = x >= 0 && x<=10 --> (checker1 x == checker2 x)