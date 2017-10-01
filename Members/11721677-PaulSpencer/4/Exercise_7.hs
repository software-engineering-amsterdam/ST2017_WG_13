module Exercise_7 where
    
import Data.List
import Test.QuickCheck
import Lecture4
import Exercise_5
import Exercise_6
import System.Random 

{-
    notes for this exercise can be found at:
    https://github.com/software-engineering-amsterdam/ST2017_WG_13/tree/master/Team/4/Exercise_7.md
-}

-- tests 

prop_SymClos :: Ord a => Rel a -> Bool
prop_SymClos r = isSubset r s && isSym r s && isMinSym r s where s = symClos r
test_SymClos = relTester 1 100 (prop_SymClos :: Rel Int -> Bool)
test_SymClos' = verboseCheck (prop_SymClos :: Rel Int -> Bool)

prop_TrClos :: Ord a => Rel a -> Bool
prop_TrClos r = isSubset r s && isMinTran r s && transCheck s where s = trClos r
test_TrClos = relTester 1 100 (prop_TrClos :: Rel Char -> Bool)
test_TrClos' = verboseCheck (prop_TrClos :: Rel Char -> Bool)

-- helpers

isSubset :: Ord a => Rel a -> Rel a -> Bool
isSubset [] _ = True
isSubset ((x,y):xs) ys
        | ((x,y) `elem` ys) = isSubset xs ys
        | otherwise = False

isSym :: Ord a => Rel a -> Rel a -> Bool
isSym [] _ = True
isSym ((x,y):xs) ys 
            | (elem (x,y) ys) && (elem (y,x) ys) = isSym xs ys
            | otherwise = False

isMinSym :: Ord a => Rel a -> Rel a -> Bool
isMinSym r1 r2 = null $ filter (\(a,b) -> (b,a) `notElem` r1) (r2 \\ r1)

isMinTran :: Ord a => Rel a -> Rel a -> Bool
isMinTran r1 r2 = null $ filter (\pair -> transCheck (delete pair r2)) (r2 \\ r1)

-- from scratch tester
relGen::(Random a, Show a) => Int -> IO (Rel a)
relGen n = do
    l <- getStdRandom (randomR (0,n))
    g <- getStdGen
    return $ take l $ zip (randoms g) (randoms g)

relTester ::(Random a, Show a) =>  Int -> Int -> (Rel a -> Bool) -> IO ()
relTester current total property = 
    if current == total then 
        print (show total ++ " tests passed")
    else do
        rel <- relGen 50
        if property rel then
            do 
                print ("pass on: " ++ show rel)
                relTester (current+1) total property
        else 
            error ("failed test on: " ++ show rel)
