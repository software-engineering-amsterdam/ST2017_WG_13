module Lab2_Exercise4 where
  
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
    
-- Recognizing Permutations

-- A permutation of a finite list is another finite list with the same elements, but possibly in a different 
-- order. For example, [3,2,1] is a permutation of [1,2,3], but [2,2,0] is not. Write a function

--  isPermutation :: Eq a => [a] -> [a] -> Bool
-- that returns True if its arguments are permutations of each other.

-- Next, define some testable properties for this function, and use a number of well-chosen lists to test 
-- isPermutation. You may assume that your input lists do not contain duplicates. What does this mean for 
-- your testing procedure?

-- Provide an ordered list of properties by strength using the weakear and stronger definitions.

-- Can you automate the test process? Use the techniques presented in this week's lecture. Also use QuickCheck.

-- Deliverables: Haskell program, concise test report, indication of time spent.




isPermutation :: Eq a => [a] -> [a] -> Bool


-- 
-- The LEFT -<= Right operator means something like 
-- all elements of left are element of right

left -<= right = foldl ( \acc x -> acc && x `elem` right) True left

left =>- right = right -<= left

left -<==>- right = left =>- right && left -<= right 
              
isPermutation a1 a2 = length a1 == length a2  && a1 -<==>- a2 

            
    
