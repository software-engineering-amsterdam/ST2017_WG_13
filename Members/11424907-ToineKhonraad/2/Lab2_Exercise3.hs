module Lab2_Exercise3 where
  
{-
------------------------------------------------------------------
Testing properties strength

Considering the following predicate on test properties:

> stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
> stronger xs p q = forall xs (\ x -> p x --> q x)
> weaker   xs p q = stronger xs q p 


a) Implement all properties from the Exercise 3 from 
   Workshop 2 as Haskell functions of type Int -> Bool. 
   Consider a small domain like [(−10)..10].

b) Provide a descending strength list of all the implemented properties.

------------------------------------------------------------------


-}

import Data.List

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)
--weaker   xs p q = stronger xs q p 
weaker   xs p q = not (stronger xs p q)
--------------------------------------------- ! ! ! ! ! ! HELP 

quicksort  :: (Ord a) =>  [a] -> [a]
quicksort []     =  []
quicksort (x:xs) =  quicksort ( filter (<= x) xs )
                    ++ 
                    [x]
                    ++ 
                    quicksort ( filter (> x) xs )

testList :: (Enum t, Num t) => [t]
testList = [-10..10]

theList :: [ ( Int, (Integer -> Bool) ) ]
theList = [ 
    ( 1, even ),
    ( 2, (\ x -> even x && x > 3) ),
    ( 3, (\ x -> even x || x > 3) ),
    ( 4, (\ x -> (even x && x > 3) || even x) )

  ] 
           


stronger' :: [a2] -> (a1, a2 -> Bool) -> (a, a2 -> Bool) -> Bool
stronger' l p1 p2 = stronger l (snd p1) (snd p2)

weaker' :: [a2] -> (a1, a2 -> Bool) -> (a, a2 -> Bool) -> Bool
weaker'   l p1 p2 = weaker   l (snd p1) (snd p2)

quicksort'' :: (Num a, Enum a) => [(a1, a -> Bool)] -> [(a1, a -> Bool)]
quicksort'' []     =  []
quicksort'' (x:xs) =  quicksort'' strongerthan  ++ [x] ++ quicksort'' weakerthan 
                      where
                            strongerthan = filter ( stronger' testList x ) xs
                            weakerthan   = filter ( weaker'   testList x ) xs 
  
 

