module Lab1 where 
import Data.List
import Test.QuickCheck

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

xor :: Bool -> Bool -> Bool
a `xor` b = (a || b) && not ( a && b )

accuses :: Boy -> Boy -> Bool
accuses Matthew x = (x == Jack) || (x == Peter) || (x == Arnold)
accuses Peter x = (x == Matthew) || (x == Jack)
accuses Jack x = (not $ accuses Matthew x) && (not $ accuses Peter x)
accuses Arnold x = (accuses Matthew x) `xor` (accuses Peter x) 
accuses Carl x = not (accuses Arnold x)

-- Accusers is the list of boys accusing the selected boy, so a list 
-- comprehension of all the boys who accuse x makes sense
accusers :: Boy -> [Boy]
accusers accused = [ boy | boy <- boys, accuses boy accused]  

accusations :: [ ( Boy, [Boy] ) ]                        
accusations = [ (boy, accusers boy) |  boy <- boys ]

thief :: Boy
thief = fst $ head $ filter ( \x -> length ( snd x ) == 3 ) accusations 

honest :: [Boy]
honest = snd $ head $ filter ( \x -> length ( snd x ) == 3 ) accusations  

{-
notes: 
the consensus was to use a truthtable as a function of propositions. 
the minority opinion was Paul's version that split the problem in half, by using  
Jack, Carl and Arnold to determine who is telling the truth and Matthew and Peters statements to find
who the thief was.
-}