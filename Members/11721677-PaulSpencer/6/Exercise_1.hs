module Exercise_1 where

multM :: Integer -> Integer -> Integer -> Integer
multM x y = rem (x*y) 
  
expM::Integer -> Integer -> Integer -> Integer
expM x y m 
    | y == 0    = 1
    | y == 1    = x `mod` m 
    | odd y     = multM x (expM x (y-1) m) m
    | otherwise = expM (multM x x m) (y `div` 2) m