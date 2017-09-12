module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Q2

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
   | not (isTriangle a b c) = NoTriangle
   | isEquilateral a b c = Equilateral
   | isIsosceles a b c = Isosceles
   | isRectangular a b c = Rectangular
   | otherwise = Other

isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = ((a + b) >= c) || ((a + c) >= b) || ((b + c) >= a)

isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = a == b && a == c

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c = (a == b) || (a == c) || (b == c)

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = (a^2 + b^2) == c^2 || (b^2 + c^2) == a^2 || (a^2 + c^2) == b^2