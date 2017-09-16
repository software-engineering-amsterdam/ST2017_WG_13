module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Q2

-- Program 
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = triangleType (sort [a,b,c])

triangleType :: [Integer] -> Shape
triangleType [a,b,c]
   | not (isTriangle a b c) = NoTriangle
   | isEquilateral a b c = Equilateral
   | isRectangular a b c = Rectangular
   | isIsosceles a b c = Isosceles
   | otherwise = Other

{- Triangle Classification Properties -}

-- A triangle is defined by the sum of the smaller sides being greater than the largest side.
isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = ((a /= 0) && (b /= 0) && (c /= 0)) && ((a + b) > c)

-- All sides of triangle are same length
isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = a == b && b == c

-- Two sides are the same length
isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c = (a == b) || (b == c) 

-- Pythagorean Theorem
isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = (a^2 + b^2) == c^2

{-
Test Procedure: 

Testing the correctness of the triangle program is done by testing the order in which inputs
are received. This correctness lies in the property that triangles with the same side lengths 
belong to the same triangle classification, no matter the ordering of the sides (i.e. the triangles
rotation along its centerpoint).

To perform this test, the derangement set of the input is used to compare their resulting shape
with that of the original input.

A passing test shows a differing order of inputs produces equivalent shaped triangles.
A failed test shows a differing order of inputs produces inequivalent shaped triangles.
-}

triangleTest :: Integer -> Integer -> Integer -> Bool
triangleTest a b c = (triangle a b c == triangle b c a) && (triangle a b c == triangle c a b)

{-
Test report:

quickCheck on the triangleTest property passes all tests.

First 10 checks are shown below with the corresponding input:

1. 
Passed:
0
0
0

2. 
Passed:
1
-1
-1

3. 
Passed:
0
0
0

4. 
Passed:
1
-1
-2

5. 
Passed:
1
2
-4

6. 
Passed:
0
4
4

7. 
Passed:
-3
-6
3

8. 
Passed:
-7
-5
-1

9. 
Passed:
4
0
-7

10. 
Passed:
-7
-4
0


Total Time Spent: ~1.5 hrs
-}