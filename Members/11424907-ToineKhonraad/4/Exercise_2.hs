module Exercise_2 where
  
  import System.Random
  import SetOrd
  import Test.QuickCheck
  import Data.List
  import Data.Either

  -------------------------------------------------------------------------
  -- Generators from scratch
  -------------------------------------------------------------------------
  
  -- define random Int generator
  --
  randomNumber :: Int -> Int -> IO Int
  randomNumber low high = do
    r <- getStdRandom (randomR (low, high))
    return r

  -- define random [Int] generator
  --
  randomListOfInt :: Int -> Int -> IO [Int]
  randomListOfInt 0 _ = return []
  randomListOfInt size scale = do
    p   <- randomNumber (-scale) scale
    ps  <- randomListOfInt (size-1) scale
    return (p:ps)

  -- define random Set Int generator (using just list2set from SetOrd.hs)
  --
  randomSet :: [Int] -> Set Int
  randomSet = list2set


  -------------------------------------------------------------------------
  -- Testable properties
  -------------------------------------------------------------------------

  prop_UnionSelf :: Set Int -> Bool
  prop_UnionSelf set =  (unionSet set set) == set

  prop_Unique :: Set Int -> Bool
  prop_Unique  (Set xs) = length (nub xs) == length xs

  prop_Complete ::  [Int] -> Set Int -> Bool
  prop_Complete list set = all ( \x -> inSet x set ) list

  prop_OrderAgnostic ::  [Int] -> Set Int -> Bool
  prop_OrderAgnostic list set = (list2set list) == (list2set (reverse list))

  -------------------------------------------------------------------------
  -- Test properties using scratch generators
  -------------------------------------------------------------------------

  testFromScratch = do
    
    list <- randomListOfInt 10 100

    let set  = randomSet list
    
    let result  = (prop_UnionSelf set)  &&
                  (prop_Unique set) &&
                  (prop_Complete list set) &&
                  (prop_OrderAgnostic list set)

    if ( result ) then print "All tests succeeded"
                  else print "Test(s) failures"

      
  -------------------------------------------------------------------------
  -- Test properties using QuickCheck
  -------------------------------------------------------------------------

  instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = sized (\n -> do
        lst <- choose (0,n) >>= vector
        return $ list2set lst)

  testQuickCheck = do
  
    do
      quickCheck  prop_UnionSelf 
      quickCheck  prop_Unique

  -------------------------------------------------------------------------
  -- To test all goto ghci and type:
  --
  --    testFromScratch
  --
  --    and
  --
  --    testQuickCheck
  -------------------------------------------------------------------------
