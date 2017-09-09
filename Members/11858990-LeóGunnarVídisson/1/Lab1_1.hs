module Lab1_1 where
import Test.QuickCheck

--Part one
{-
  Time: 3-5 Hours
  This took way to long time, beginners strugle
-}

--a
list1 :: Integer -> Integer
list2 :: Integer -> Integer
list1 k = sum [n^2 | n <- [0..k]]
 
list2 k = (k * (k + 1) * (2 * k + 1))`div`6
lab1a x = list1 (abs(x)) == list2 (abs(x))

--b
{-
  Time: 15 min
  More or less the same as part a.
-}

listA :: Integer -> Integer
listB :: Integer -> Integer

listA k = sum [n^3 | n <- [0..k]]
listB k = (k * (k + 1)`div`2)^2

lab1b x = listA (abs(x)) == listB (abs(x))








