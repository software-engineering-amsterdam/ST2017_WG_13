module Lab1 where 
import Data.List
import Test.QuickCheck

-- Matthew: Carl didn't do it, and neither did I.
-- Peter: It was Matthew or it was Jack.
-- Jack: Matthew and Peter are both lying.
-- Arnold: Matthew or Peter is speaking the truth, but not both.
-- Carl: What Arnold says is not true.
-- three of these boys always tell the truth, and two always lie

--([Matthew,Peter,Jack],[Arnold,Carl]) -- carl says arnold is a liar 
--([Peter,Jack,Arnold],[Matthew,Carl]) -- jack says mathew and peter are both lying
--([Matthew,Peter,Arnold],[Jack,Carl]) -- arnold says XOR mathew peter
--([Matthew,Jack,Arnold],[Peter,Carl]) -- jack says mathew and peter are both lying
--([Jack,Arnold,Carl],[Matthew,Peter]) -- carl says arnold is a liar
--([Peter,Jack,Carl],[Matthew,Arnold]) -- jack says mathew and peter are both lying
--([Peter,Arnold,Carl],[Matthew,Jack]) -- carl says arnold is a liar
--([Matthew,Peter,Carl],[Jack,Arnold]) OK!
--([Matthew,Jack,Carl],[Peter,Arnold]) -- jack says mathew and peter are both lying
--([Matthew,Arnold,Carl],[Peter,Jack]) -- jack says mathew and peter are both lying

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show,Ord)
boys = [Matthew, Peter, Jack, Arnold, Carl]


--qsort inspired from Programming in Haskell, Graham Hutton chapter 1
qsort::Ord a=> [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort smaller) ++ [x] ++ (qsort larger)
        where 
            smaller = [a | a <- xs, a <= x]
            larger =  [b | b <- xs, b > x]

truthLieCombos = nub [(qsort (take 3 xs), qsort (drop 3 xs)) | xs <- permutations boys]

validSet::([Boy],[Boy]) -> Bool
validSet (ts, fs) 
    | t Carl && t Arnold                = False
    | f Carl && f Arnold                = False
    | t Arnold && t Matthew  && t Peter = False
    | t Arnold && f Matthew  && f Peter = False
    | f Arnold && t Matthew  && f Peter = False
    | f Arnold && f Matthew  && t Peter = False
    | t Jack && (t Matthew || t Peter)  = False
    | otherwise                         = True
        where 
            t b = elem b ts
            f b = elem b fs

suspects::([Boy],[Boy]) -> [Boy]
suspects (ts, fs) 
    | t Matthew && t Peter   = tMatthew $ tPeter boys
    | t Matthew && f Peter   = tMatthew $ fPeter boys
    | f Matthew && t Peter   = fMatthew $ tPeter boys
    | f Matthew && f Peter   = fMatthew $ fPeter boys
    where 
        t b = elem b ts
        f b = elem b fs
        tMatthew bs = filter (/=Matthew) $ filter (/=Carl) bs
        fMatthew bs = filter (/=Jack) $ filter (/=Jack) $ filter (/=Peter) bs
        tPeter bs = filter (/=Carl) $ filter (/=Arnold) $ filter (/=Peter) bs
        fPeter bs = filter (/=Matthew) $ filter (/=Jack) bs

        
validLiars = filter validSet truthLieCombos
jackHughes = map suspects validLiars

{-
this one was particuarly hard for me as I cound not get to grips with what I saw as ambiguities.
I could not also make the leap that there had to be 3 people accusing to be the 3 truth tellers and
that not giving an allibi was the same as accusing, so I decided to reduce the search space of truth tellers and
liars by getting all possible combinations and using the statements of the boys to find out who was telling
the truth and who was lying.
once I had done this it is easy to filter based on the only two people that make direct statements (peter and
Matthew) rather than those who talk about who is telling the truth.

this took me way too long (no idea exactly how long) and the outcome is not very flexible.  
not my proudest moment!

-}