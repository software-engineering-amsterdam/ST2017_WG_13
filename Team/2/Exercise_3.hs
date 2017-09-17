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

import Lecture2

data Strength = Equivalent | Stronger | Weaker | Incomparable deriving (Eq, Ord, Show,  Enum)
  
propertyList :: [ ( Int, (Integer -> Bool) ) ]

propertyList = [ 
            {-
              ----------------------------------------------------
              label   property
              ----------------------------------------------------
            -}
            (   1,      even                                  ),
            (   2,      (\ x -> even x && x > 3)              ),
            (   3,      (\ x -> even x || x > 3)              ),
            (   4,      (\ x -> (even x && x > 3) || even x)  ),
            (   5,      (\ x -> x > 3 )                       ) 
          ] 
           
{-
  The fifth property given in the list above is added so to atleast have one that is 
  incomparable to the others.
-}


comparePropertyForList :: [a] -> (a -> Bool) -> (a -> Bool) -> Strength
comparePropertyForList xs p q = 

                let  pq = stronger xs p q 
                     qp = stronger xs q p 
                in 
                  if pq && qp then Equivalent
                  else if pq  then Stronger
                  else if qp  then Weaker
                  else             Incomparable


  
quicksort' :: (Num a, Enum a) => [a] -> [(a1, a -> Bool)] -> [(a1, a -> Bool)]
quicksort' domain []     =  []
quicksort' domain (x:xs) =  quicksort' domain [ a | a <- xs, comparePropertyForList domain (snd x) (snd a)  == Equivalent  ] 
                     ++
                     quicksort' domain [ a | a <- xs, comparePropertyForList domain (snd x) (snd a)  == Stronger  ] 
                     ++
                     [x]                       
                     ++ 
                     quicksort' domain [ a | a <- xs, comparePropertyForList domain (snd x) (snd a)  == Weaker  ] 
                     ++ 
                     quicksort' domain [ a | a <- xs, comparePropertyForList domain (snd x) (snd a)  == Incomparable  ] 


showordered domain = [ fst x  | x <- quicksort' domain propertyList ] 

{-

Results in ghci:
-----------------------------------------------------------------
print $ showordered [-100..100]
[4,3,1,2,5]

print $ showordered []
[5,4,3,2,1]

print $ showordered [   3..100]
[4,2,5,3,1]

print $ showordered [-100..0]
[4,3,1,5,2]

-}


-- Time spent: 3 hours
