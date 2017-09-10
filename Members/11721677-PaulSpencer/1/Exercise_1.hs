module Lab1 where 
import Data.List
import Test.QuickCheck
import Numeric.Natural -- to use the natural numbers type

-- Exercise 1 Redo exercises 2 and 3 of Workshop 1 by writing QuickCheck tests for these statements. 
-- See the end of Lecture 1 for how this can be done.

-- Workshop 1, Question 2:
--Prove by induction that it holds for all natural numbers n that
--    (1^2)+(2^2)+⋯+(n^2)=(n(n+1)(2n+1))/6.

-- the Arbitary instance of Natural is required for quickCheck's random test generation
-- arbitrarySizedNatural is part of the quickCheck library
instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural 

lhsQ2::Natural -> Natural
lhsQ2 k = sum [n^2 | n <- [0..k]]

rhsQ2::Natural -> Natural
rhsQ2 k = (k * (k + 1) * ((2 * k)+1)) `div` 6

propertyHoldsQ2::Natural -> Bool
propertyHoldsQ2 k = lhsQ2 k == rhsQ2 k

-- Workshop 1, Question 3:
-- Prove by induction that it holds for all natural numbers n that
--    (1^3)+(2^3)+⋯+(n^3)=((n(n+1))/2)^2.

lhsQ3::Natural -> Natural
lhsQ3 k = sum [n^3 | n <- [0..k]]

rhsQ3::Natural -> Natural
rhsQ3 k = ((k*(k+1)) `div` 2) ^ 2

propertyHoldsQ3::Natural -> Bool
propertyHoldsQ3 k = lhsQ3 k == rhsQ3 k

runTests = $quickCheckAll

{-
Retrospective:

I went a different way to the rest of the team.
The induction hypothesis can be proved by using a precondition in the property to limit the test 
set. 
In these two examples, where one is only causeing ~half of the tests to return postitive results
when no actual test is run, the problem is negligable, however a precondition that cuts the testable
domain down much more dramatically (as we see in later exercises) can cause quickcheck to return 100
positive results when all tests only test the precondition and none the actual code under test. To overcome
this problem I could have used quickChecks inbuilt preconditions.

My decision was that if I was going to test something I would test that thing.  In this case that thing
was Natural numbers.  
By putting the limits into the type and not into the test, we are causing 100% of the tests run to be 
actually testing the code under test and, Much more importantly, preventing any future users, 
(including ourselves), who have access to the code but not the test, from using the code incorrectly, 
and even more importantly thinking that we are idiots for writing crappy code that fails, when it is infact
them who are idiots for just copying and pasting the code without properly understanding the context.

I chose to limit the code using the Natural Type from the Numeric.Natural library.  if you are to run this
code and you have not done a cabal-install of this library from Hackage, then I guess this will not work.
In order to get this to work I had to add an Arbitary Class for Natural, so that quickCheck could create
random numbers. Luckily quickCheck already had a arbitrarySizedNatural implementation, so I did not have to
think too much about that.

This choice has other issues, such as without investigation, I do not know whether Numeric.Natural is as 
thoroughly tested as Int, and because "onder de water" it is building up using Succ there is a strong 
probability that it is far less performant than using Int. 

Total Time Spent: Waaaaaayyyy too long. going back and forth arguing about what was the right way to do
this. I did not record actual time
-}