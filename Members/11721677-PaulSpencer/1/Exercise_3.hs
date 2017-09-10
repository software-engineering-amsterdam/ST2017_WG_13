module Lab1 where 
import Data.List
import Test.QuickCheck

-- Redo exercise 5 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of 
-- the form [1..n].
-- Is the property hard to test? If you find that it is, can you given a reason why?
-- Again, give your thoughts on the following issue: when you perform the test for exercise 5, what are you 
-- testing actually? Are you checking a mathematical fact? Or are you testing whether perms satisfies a 
-- part of its specification? Or are you testing something else still?

-- My big problem with this was understanding the first sentence of the question.
-- initially, after seeking advice from the TA, I took it to mean test if perms and permutations are the same
-- and if not why is this a problem.  this was quickly done as follows:

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

setsMatchProperty::Int -> Bool
setsMatchProperty k = all (==True) $ zipWith (==) (perms [1..k]) (permutations [1..k])

-- Whilst we would expect these to match if they were sets, they do not when they are lists because the
-- identity of a set does not take into account the order of it's memebers, whereas the identity of a list 
-- does

-- however it was suggested that the actual thing we should be testing was:
-- Find a formula (closed form) for the number of permutations of a list of nn distinct objects, and prove your guess by induction.
-- There are n! permutations for a list of n distinct objects. The empty list has a single permutation, and indeed, 0!=10!=1 (by the convention for an empty product).
-- Suppose a list of nn distinct objects has n!n! permutations. Then there are n+1n+1 ways to insert a new object into one of these. Together this gives (n+1)×n!=(n+1)!(n+1)×n!=(n+1)! permutations of a list of n+1n+1 distinct elements.

-- I had another implementation but tobiassjosten.net/haskell/factorials-in-haskell/ has a simpler one
factoral::Int -> Int
factoral n = product [1..n] 

inductionHypothesisEx3::Int -> Int
inductionHypothesisEx3 n = factoral n

baseCaseEx3::Int
baseCaseEx3 = length $ permutations []

inductionCaseEx3::Int -> Int
inductionCaseEx3 k = length $ permutations [1..k]

permsFactoralProperty::Int -> Bool
permsFactoralProperty k = inductionCaseEx3 k == inductionHypothesisEx3 k
