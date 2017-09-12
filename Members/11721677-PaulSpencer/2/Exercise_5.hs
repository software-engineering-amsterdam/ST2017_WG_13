module Lab2 where
import Data.List
import Data.Char
import System.Random
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.Set as S

-- Recognizing and generating derangements

-- A derangement of the list [0..n-1] of natural numbers is a permutation π of the list with the property 
-- that for no x in the list π(x)=x. This is what you need if you prepare for Sinterklaas with a 
-- group of friends, where you want to avoid the situation that someone has to buy a surprise gift for him- 
-- or herself.

-- Give a Haskell implementation of a property isDerangement that checks whether one list is a derangement 
-- of another one.

-- Give a Haskell implementation of a function deran that generates a list of all derangements of the list 
-- [0..n-1].

-- Note You may wish to use the permutations function from Data.List, or the perms function from workshop 1.

-- Next, define some testable properties for the isDerangement function, and use some well-chosen integer 
-- lists to test isDerangement.

-- Provide an ordered list of properties by strength using the weakear and stronger definitions.

-- Can you automate the test process?

-- Deliverables: Haskell program, concise test report, indication of time spent.

deran::Int -> [[Int]]
deran n = filter areSwapped $ permutations [0.. (n-1)]
deran' n = [0.. (n-1)]

areSwapped::[Int] -> Bool
areSwapped perm = all (==False) $ zipWith (==) perm [0..]

isDerangement::[Int] -> [[Int]] -> Bool
isDerangement [] [[]] = True
isDerangement [_] [] = True
isDerangement os@(x:xs) dss@(ys:yss)
    | (derangementSize (length os)) /= (length dss)           = False
    | not $ all (==(length os)) (map length dss)              = False
    | any (==True) $ concat $ map (zipWith (==) os) dss       = False
    | not $ all (==True) $ map (sameElems os) dss             = False
    | otherwise  = True


-- inspired by Ben Millwood's answer https://stackoverflow.com/questions/15319136/how-to-compare-two-lists-in-haskell
sameElems xs ys = S.fromList xs == S.fromList ys


-- formula for derangement size taken from http://oeis.org/wiki/Number_of_derangements
derangementSize 1 = 0 
derangementSize n = ((derangementSize (n-1)) * n) + ((-1)^n)

tmp os@(x:xs) dss@(ys:yss) = concat $ map (zipWith (==) os) dss

    -- | any $ concat $ map (zipWith (==) os) dss      = False
    -- | all $ concat $ zipWith (elem os)

-- implement https://link.springer.com/content/pdf/10.1007/BF01933579.pdf to prove