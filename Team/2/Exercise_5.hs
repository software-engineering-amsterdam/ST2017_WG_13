module Lab2 where
import Data.List
import Data.Char
import System.Random
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.Set as S
import Data.Map (fromListWith, toList)

-- Give a Haskell implementation of a function deran that generates a list of all derangements of the list 
-- [0..n-1].

-- Next, define some testable properties for the isDerangement function, and use some well-chosen integer 
-- lists to test isDerangement.
-- Provide an ordered list of properties by strength using the weakear and stronger definitions.

-- Deliverables: Haskell program, concise test report, indication of time spent.

{-
  notes: a lot of time was mis-spent on testing deran and not on testing isDerangement (see Pauls code for
  a journey down that rabbit hole)

  To achieve Testing we create pseudo random derangements and then deform them in specific ways to make them 
  fail and check that the failures fail.
-}

deran::Integer -> [[Integer]]
deran n = filter allDifferent $ permutations [0.. (n-1)]
    where 
        allDifferent perm = all (==False) $ zipWith (==) perm [0..]

iLength = (toInteger . length)

-- formula for derangement size taken from http://oeis.org/wiki/Number_of_derangements
derangementSize::Integer -> Integer
derangementSize 0 = 1 
derangementSize 1 = 0 
derangementSize n = ((derangementSize (n-1)) * n) + ((-1)^n)

-- lets start again with is derangement (this assumes no duplicates)
isDerangement xs ys
    | length xs == 1 && null ys            = True
    | length xs /= length ys               = False
    | (S.fromList xs) /= (S.fromList ys)   = False 
    | any (==True) $ zipWith (==) xs ys    = False
    | otherwise                            = True

-- Tests
-- Limit the inputs to be only lists of integers that we will shorten to between 2 and 9 
-- and then pick one of first 300 derangements generated
listGen:: Gen ([Integer], Integer, Integer)
listGen = tripple <$> arbitrary <*> choose (2,9) <*> choose (0,300)
tripple a b c = (a,b,c)

-- and make sure we only test lists larger than 1 that have no duplicates 
precon xs = (length xs > 1) && (length xs == (length (nub xs))) 

-- Tests Weakest to Strongest: 
-- test that if a returned item is bigger than the original it is not a derangement
prop_testTooBig_limited = forAll listGen prop_testTooBig

-- test that if a returned item is smaller than the original it is not a derangement
prop_testTooSmall_limited = forAll listGen prop_testTooSmall

-- test that if a returned item is same size but contains a different item it is not a derangement
prop_testDifferent_limited = forAll listGen prop_testDifferent

-- test that if a returned item is same size but one or more of the places match
prop_placeMatch_limited = forAll listGen prop_placeMatch

-- test that helpers generate derangements as per desription
prop_testTest_limited = forAll listGen prop_testTest


prop_testTest::([Integer], Integer, Integer) -> Property
prop_testTest (xs, len, n) = (length xs > 0) ==> isDerangement os ds
    where
        (os, ds) = getDerangement xs len n

prop_testTooSmall::([Integer], Integer, Integer) -> Property
prop_testTooSmall (xs, len, n) = precon xs ==> not $ isDerangement os (drop 1 ds)
    where
        (os, ds) = getDerangement xs len n

prop_testTooBig::([Integer], Integer, Integer) -> Property
prop_testTooBig (xs, len, n) = precon xs  ==> not $ isDerangement os (ds ++ (addUniqueNum os ds))
    where
        (os, ds) = getDerangement xs len n

addUniqueNum os ds = ds ++ (take 1 $ [1..] \\ os)

prop_testDifferent::([Integer], Integer, Integer) -> Property
prop_testDifferent (xs, len, n) = precon xs  ==> not $ isDerangement os ((addUniqueNum os ds) ++ (drop 1 ds))
    where
        (os, ds) = getDerangement xs len n

prop_placeMatch::([Integer], Integer, Integer) -> Property
prop_placeMatch (xs, len, n) = precon xs  ==> not $ isDerangement os ds
    where
        (os, ds) = getArrangement xs len n

-- In Order To Test we have some helpers: to get valid derangements and arangements

getDerangement::Ord a => [a] -> Integer -> Integer -> ([a],[a])
getDerangement xs len n = (xs'', concat $ take 1 $ drop n' [ys | ys <- permutations xs'', all (==False) $ zipWith (==) xs'' ys])
    where 
        len' = fromInteger len
        xs' = nub xs
        xs'' = if (len' >= (length xs')) then xs' else take (len' `mod` (length xs')) xs'
        dsize = (derangementSize (iLength xs''))
        n' = if dsize == 0 then 0 else fromInteger $ n `mod` dsize

getArrangement::Ord a => [a] -> Integer -> Integer -> ([a],[a])
getArrangement xs len n = (xs'', concat $ take 1 $ drop n' [ys | ys <- permutations xs'', any (==True) $ zipWith (==) xs'' ys])
    where 
        len' = fromInteger len
        xs' = nub xs
        xs'' = if (len' >= (length xs')) then xs' else take (len' `mod` (length xs')) xs'
        dsize = (derangementSize (iLength xs''))
        n' = if dsize == 0 then 0 else fromInteger $ n `mod` dsize        