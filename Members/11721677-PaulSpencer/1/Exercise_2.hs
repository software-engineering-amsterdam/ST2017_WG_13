module Lab1 where 
import Data.List
import Test.QuickCheck
import Numeric.Natural
import qualified Data.Set as Set

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural 

--a) Prove by induction A that if is a finite set with |A| = n, |P(A)| then = n^2
inductionHypothesisEx2::Natural -> Int
inductionHypothesisEx2 s = 2 ^ (toInteger s)

baseCaseEx2::Int
baseCaseEx2 = length $ subsequences []

inductionCaseEx2::Natural -> Int
inductionCaseEx2 s = length $ subsequences [1..s]

baseTestEx2::Bool
baseTestEx2 = (baseCaseEx2) == (inductionHypothesisEx2 0)

inductionPropertyEx2::Natural -> Bool
inductionPropertyEx2 s = inductionHypothesisEx2 s == inductionCaseEx2 s

-- Whilst the above is correct (assuming that subsequences is correct), the growth of the program 
-- execution is exponential.

-- when thinking about the question, what am I testing, the answer i found was: the implementation of
-- the subsequences function with lists. so I thought instead of using Lists and subsequences I should be 
-- using Sets and PowerSets, so I imported Data.Set only to discover that it did not have a powerset 
-- function, so I borrowed one: 

--powerset inspired by: Dan Burtons stackoverflow answer https://stackoverflow.com/questions/6428279/why-data-set-has-no-powerset-function
powerset s
    | s == Set.empty = Set.singleton Set.empty
    | otherwise = Set.map (Set.insert x) pxs `Set.union` pxs
        where 
            (x, xs) = Set.deleteFindMin s
            pxs = powerset xs

inductionCaseEx2_sets::Natural -> Int
inductionCaseEx2_sets k = Set.size $ powerset $ Set.fromList [1..k] 

-- propQ2_alt k = (toInteger (q2d k)) == 2 ^ (toInteger  k)
inductionPropertyEx2_sets::Natural -> Bool
inductionPropertyEx2_sets s = inductionHypothesisEx2 s == inductionCaseEx2_sets s

-- it appears that typing does not overcome the exponentiality of the compute time growth
-- so I decided to get practical and add preconditions

-- precondition (heavily) inspired by lecture 1 workshop text
infix 1 --> 
(-->)::Bool -> Bool -> Bool
p --> q = (not p) || q

-- through halving found that 30 seemed to be the maximum test group size for my implementation and hardware
inductionCaseEx2_preCon::Natural -> Bool
inductionCaseEx2_preCon s = (s < 30) --> inductionHypothesisEx2 s == inductionCaseEx2 s

-- if I decide to use ints instead I can do this:
inductionHypothesisEx2_int::Int -> Int
inductionHypothesisEx2_int s = 2 ^ s

inductionCaseEx2_int::Int -> Int
inductionCaseEx2_int s = length $ subsequences [1..s]

inductionPropertyEx2_int::Int -> Bool
inductionPropertyEx2_int s = inductionHypothesisEx2_int s == inductionCaseEx2_int s

setDomain:: Gen Int
setDomain = choose (0, 30)

inductionPropertyEx2_intAll = forAll setDomain inductionPropertyEx2_int

{-
Retrospective:

this was relatively easy to create, and to spot the problem, however I ran down a rabbit hole seeing if 
using the typesystem was a solution when did not address the main issue, i.e. the exponential growth of the 
solution space of the algotythm.

my initial response was to limit the domain with the precondition from the workshop, however I was not 
satisfied about the sucessful tests that had not tested anything. to test the entire range from (0,30) I used 
the quickcheck generator and forAll function.  this was inspired by Alexey Kuleshevich "QuickCheck and Magic of 
Testing" - https://www.fpcomplete.com/blog/2017/01/quickcheck

Total Time Spent: no idea ... who looks at a clock?
-}