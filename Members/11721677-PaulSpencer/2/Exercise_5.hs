module Lab2 where
import Data.List
import Data.Char
import System.Random
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.Set as S
import Data.Map (fromListWith, toList)

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

deran::Integer -> [[Integer]]
deran n = filter allDifferent $ permutations [0.. (n-1)]
    where 
        allDifferent perm = all (==False) $ zipWith (==) perm [0..]

deran'::Eq a => [a] -> [[a]]
deran' xs = [ys | ys <- permutations xs, all (==False) $ zipWith (==) xs ys]


iLength = (toInteger . length)

performanceLimit :: Gen Integer
performanceLimit = choose (0, 9)
orig n = [0.. (n-1)]

prop_correctNumberOfDerangements::Integer -> Bool
prop_correctNumberOfDerangements n = (derangementSize n) == (iLength $ deran n)
prop_correctNr_limited = forAll performanceLimit prop_correctNumberOfDerangements

prop_allSameSize::Integer -> Bool
prop_allSameSize n = not $ any (==True) $ concat $ map (zipWith (==) $ orig n) (deran n)   
prop_allSameSize_limited = forAll performanceLimit prop_allSameSize

prop_noItemsInSamePlace::Integer -> Bool
prop_noItemsInSamePlace n = all (==n) (map iLength $ deran n)  
prop_noItemsInSamePlace_limited = forAll performanceLimit prop_noItemsInSamePlace

prop_onlyItemsFromOriginal::Integer -> Bool
prop_onlyItemsFromOriginal n = all (<n) $ concat $ deran n  
prop_onlyItemsFromOriginal_limited = forAll performanceLimit prop_onlyItemsFromOriginal

prop_noDuplicates::Integer -> Bool
prop_noDuplicates n = let derans = deran n in (length derans) == (length $ S.fromList derans)
prop_noDuplicates_limited = forAll performanceLimit prop_noDuplicates

-- oops -> mis read question, thought this was about all derangements (this assumes unique list members)
areDerangements::[Integer] -> [[Integer]] -> Bool
areDerangements [] [[]] = True
areDerangements [_] [] = True
areDerangements os dss
    | not $ correctNumberOfResults  = False
    | not $ allSameLength           = False
    | itemsInSamePosition           = False
    | not $ onlyItemsFromOriginal   = False
    | hasDuplicates                 = False
    | otherwise                     = True
        where 
            correctNumberOfResults = (derangementSize (iLength os)) == (iLength dss)
            allSameLength = all (==(length os)) (map length dss) 
            itemsInSamePosition = any (==True) $ concat $ map (zipWith (==) os) dss 
            onlyItemsFromOriginal = all (==True) $ map (sameElems os) dss
            hasDuplicates = (length dss) /= (length $ S.fromList dss)

-- inspired by Ben Millwood's answer https://stackoverflow.com/questions/15319136/how-to-compare-two-lists-in-haskell
sameElems xs ys = S.fromList xs == S.fromList ys

-- formula for derangement size taken from http://oeis.org/wiki/Number_of_derangements
derangementSize::Integer -> Integer
derangementSize 0 = 1 
derangementSize 1 = 0 
derangementSize n = ((derangementSize (n-1)) * n) + ((-1)^n)

-- lets start again with is derangement (this assumes no duplicates)
isDerangement xs ys
    | length xs == 1 && null ys             = True
    | length xs /= length ys               = False
    | (S.fromList xs) /= (S.fromList ys)   = False 
    | any (==True) $ zipWith (==) xs ys    = False
    | otherwise                            = True

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

listGen:: Gen ([Integer], Integer, Integer)
listGen = tripple <$> arbitrary <*> choose (2,9) <*> choose (0,300)
tripple a b c = (a,b,c)

prop_testTest::([Integer], Integer, Integer) -> Property
prop_testTest (xs, len, n) = (length xs > 0) ==> isDerangement os ds
    where
        (os, ds) = getDerangement xs len n
prop_testTest_limited = forAll listGen prop_testTest

precon xs = (length xs > 1) && (length xs == (length (nub xs))) 

prop_testTooSmall::([Integer], Integer, Integer) -> Property
prop_testTooSmall (xs, len, n) = precon xs ==> not $ isDerangement os (drop 1 ds)
    where
        (os, ds) = getDerangement xs len n

prop_testTooSmall_limited = forAll listGen prop_testTooSmall

prop_testTooBig::([Integer], Integer, Integer) -> Property
prop_testTooBig (xs, len, n) = precon xs  ==> not $ isDerangement os (ds ++ (addUniqueNum os ds))
    where
        (os, ds) = getDerangement xs len n

addUniqueNum os ds = ds ++ (take 1 $ [1..] \\ os)
prop_testTooBig_limited = forAll listGen prop_testTooBig


prop_testDifferent::([Integer], Integer, Integer) -> Property
prop_testDifferent (xs, len, n) = precon xs  ==> not $ isDerangement os ((addUniqueNum os ds) ++ (drop 1 ds))
    where
        (os, ds) = getDerangement xs len n

prop_testDifferent_limited = forAll listGen prop_testDifferent


prop_placeMatch::([Integer], Integer, Integer) -> Property
prop_placeMatch (xs, len, n) = precon xs  ==> not $ isDerangement os ds
    where
        (os, ds) = getArrangement xs len n

prop_placeMatch_limited = forAll listGen prop_placeMatch
