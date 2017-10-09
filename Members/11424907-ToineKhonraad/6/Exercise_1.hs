module Exercise_1 where

import Lecture6
import Debug.Trace

{-


Implement a function

  exM :: Integer -> Integer -> Integer -> Integer
  
  that does modular exponentiation of x^y

-}



exM' _ 0 _ = 1  -- invariantly: x^0 = 1
exM' x 1 _ = x  -- invariantly: x^1 = x
exM' x y m = if (odd y) 
             then 
                    multM x (exM' x (y-1) m) m
             else 
                    exM' (multM x x m) (y `div` 2) m 
                
