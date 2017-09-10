module Lab1 where 
import Data.List
import Test.QuickCheck

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show, Ord)
boys = [Matthew, Peter, Jack, Arnold, Carl]

xor::Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

--Translated each boys statement of accusation to the corresponding logic
accuses :: Boy -> Boy -> Bool
accuses Matthew x = (x == Jack) || (x == Peter) || (x == Arnold)
accuses Peter x = (x == Matthew) || (x == Jack)
accuses Jack x = (not $ accuses Matthew x) && (not $ accuses Peter x)
accuses Arnold x = (accuses Matthew x) `xor` (accuses Peter x) 
accuses Carl x = not (accuses Arnold x)

-- Accusers is the list of boys accusing the selected boy, so a list 
-- comprehension of all the boys who accuse x makes sense
accusers :: Boy -> [Boy]
accusers x = [k | k <- boys, accuses k x]

-- If we assume three boys are telling the truth about their accusation,
-- then the guilty ones will have 3 accusers
guilty :: [Boy]
guilty = [ k | k <- boys, length (accusers k) == 3]

-- Those boys who are honest are the ones that have true accusations to those boys who are guilty
honest:: [Boy]
honest = [ x | x <- boys, y <- guilty, accuses x y]

dishonest:: [Boy]
dishonest = [x | x <- boys, y <- guilty, not (accuses x y)]

