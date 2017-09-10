module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

--Q8

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

--Translated each boys statement of accusation to the corresponding logic
accuses :: Boy -> Boy -> Bool
accuses Matthew x = (x /= Matthew) && (x /= Carl) 
accuses Peter x = (x == Matthew) || (x == Jack)
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = ((accuses Matthew x) || (accuses Peter x)) && not ((accuses Matthew x) && (accuses Peter x))
accuses Carl x = not (accuses Arnold x)

-- Accusers is the list of boys accusing the selected boy, so a list 
-- comprehension of all the boys who accuse x makes sense
accusers :: Boy -> [Boy]
accusers x = [k | k <- boys, accuses k x]

guilty, honest, dishonest :: [Boy]
-- If we assume three boys are telling the truth about their accusation,
-- then the guilty ones are the boys who have 3 truthful accusers with respect to themselves
guilty = [ k | k <- boys, length (accusers k) == 3]

-- Those boys who are honest are the ones that have true accusations to those boys who are guilty
honest = [ x | x <- boys, y <- guilty, accuses x y]
dishonest = [x | x <- boys, y <- guilty, not (accuses x y)]

{- 

Reflection:

This was a pretty straightforward problem, the only difficult part was working out the
logic of the boys accusations. Once that logic was correct, the following list comprehensions
to determine who was guilty, and who were honest / dishonest was trivial.

Total Time Spent: 40 mins 

-}