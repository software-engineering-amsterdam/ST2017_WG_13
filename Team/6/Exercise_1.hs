module Exercise_1 where

  import Test.QuickCheck

  --  multM copied from Lecture6
  --
  multM :: Integer -> Integer -> Integer -> Integer
  multM x y = rem (x*y)

  --  expM copied from Lecture6
  --
  expM ::  Integer -> Integer -> Integer -> Integer
  expM x y = rem (x^y)


  ---------------------------------------------------------------------------
  -- IMPLEMENTATION
  ---------------------------------------------------------------------------
  exM :: Integer -> Integer -> Integer -> Integer
  exM _ 0 _ = 1       -- invariantly: x^0 = 1
  exM x 1 m = x `mod` m  
  exM x y m = if (odd y) 
              then  multM x (exM x (y-1) m) m
              else  exM (multM x x m) (y `div` 2) m 

  ---------------------------------------------------------------------------
  -- TESTING
  ---------------------------------------------------------------------------

  type P = Positive

  -- (Test property 1)
  --
  -- The computed result should off course be the same as the original
  -- implementation (that just called expM).

  prop_1  :: P Integer -> P Integer -> P Integer -> Bool
  prop_1 (Positive a) (Positive b) (Positive c) = (exM a b c) == (expM a b c) 


  -- (Test property 2)
  --
  -- The computed result should always be smaller then the modulus.
  
  prop_2 :: P Integer -> P Integer -> P Integer -> Bool
  prop_2 (Positive a) (Positive b) (Positive c) = (exM a b c) < c

 
  ---------------------------------------------------------------------------
  -- TEST RESULTS
  ---------------------------------------------------------------------------
  
  {-
  
    Run QuickCheck to test properties of this implementation:
     
      quickCheckResult prop_1
        
        +++ OK, passed 100 tests.

      quickCheckResult prop_2
      
        +++ OK, passed 100 tests.

  N.B.
  
  As all tests succeeded, copied implementation to "Lecture6_Refactored.hs"
  to use that in other exercises.

  -}
  
  



