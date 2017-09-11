module Lab2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
-- Testing properties strength
-- Considering the following predicate on test properties:

-- > stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
-- > stronger xs p q = forall xs (\ x -> p x --> q x)
-- > weaker   xs p q = stronger xs q p 

-- a) Implement all properties from the Exercise 3 from Workshop 2 as Haskell functions of type Int -> Bool. 
-- Consider a small domain like [(−10)..10][(−10)..10].
-- b) Provide a descending strength list of all the implemented properties.

