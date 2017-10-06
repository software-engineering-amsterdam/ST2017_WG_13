module Exercise_3

where 

import Data.List
import System.Random
import Lecture5


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


primeOfNotUnique :: Node -> Bool
primeOfNotUnique n = not (uniqueSol np) 
                where np = eraseN n (filledPositions (fst n) !! 0)

minimal :: Node -> Bool
minimal n = uniqueSol n --> primeOfNotUnique n

