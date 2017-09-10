module Lab1_3 where
import Test.QuickCheck

--Part three
{-
  Time: 3- Hours 
  Við förum í gegnum perms faillið jafn oft og 10!
  http://vvv.tobiassjosten.net/haskell/factorials-in-haskell/  

-}

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
   insrt x [] = [[x]]
   insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

test n = length (perms [1..n]) == product[1..n]