module Exercise_3 where

prime:: Integer -> Bool
prime x = null $ filter (\y ->  x `mod` y == 0) $ takeWhile (\y ->  y*y <= x) [2..]

composites:: [Integer]
composites = [x | x <- [2..], not $ prime x]