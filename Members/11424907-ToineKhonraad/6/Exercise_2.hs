module Exercise_2 where

  import Text.Printf
  import System.CPUTime
  import Test.QuickCheck
  
  import Lecture6_Refactored
  
  timedRun :: Integer -> Integer -> Integer -> IO ()
  timedRun a b c =  do
      
    --
    -- Run original
    --
    start <- getCPUTime
  
    let rv = expM a b c
  
    printf "%3d\t\t" rv
      
    end   <- getCPUTime
      
    let diff = (fromIntegral (end - start))/ (10^12)
    
    printf "%0.6f\t\t" (diff :: Double)
  
    --
    -- Run new implementation
    --
     
    start <- getCPUTime
  
    let rv2 = exM a b c
  
    printf "%3d\t\t" rv2
     
    end   <- getCPUTime
      
    let diff = (fromIntegral (end - start))/ (10^12)
    
    printf " %0.6f\n"   (diff :: Double)
  
    --
    -- Return 
    --    
    return ()
  
  --
  -- showTimingDifferences runs both implentations on growing
  -- exponents showing the (massive) time difference
  -- (experimentally found n=25 (2 ^ 25 - 1 = 33554431)
  -- to be the practical limit.
  -----------------------------------------------------------------------
  showTimingDifferences
    
    n | n <= 24 = do
          
        printf "%d\t" n
        timedRun (2^n-1)    (2^n-1)    31;
    
        showTimingDifferences (n+1)
  
        return ()
  
      | otherwise = return ()
  
  measure = do
    
    printf "-------------------------------------------\n"
    printf "n    rv    expM (sec)     rv      exM (sec)\n" 
    printf "-------------------------------------------\n"
    
    showTimingDifferences 1
    
    printf "-------------------------------------------\n"
    printf "                                   Table 1.\n"
    
    
    return ()
    
    
  {-
  
    To run just type "measure" in ghci:
    

    measure
    -------------------------------------------
    n    rv    expM (sec)     rv      exM (sec)
    -------------------------------------------
    1    1       0.000138      1       0.000076
    2   27       0.000065     27       0.000093
    3   28       0.000104     28       0.000109
    4   30       0.000097     30       0.000111
    5    0       0.000108      0       0.001295
    6    1       0.000075      1       0.000077
    7   17       0.000058     17       0.000080
    8    1       0.000057      1       0.000075
    9   15       0.000061     15       0.000308
    10   0       0.000066      0       0.000079
    11   1       0.000311      1       0.000080
    12  30       0.000145     30       0.000082
    13   7       0.001158      7       0.000085
    14  27       0.001277     27       0.000286
    15   0       0.003681      0       0.000315
    16   1       0.010635      1       0.000148
    17   3       0.020319      3       0.000218
    18   2       0.034470      2       0.000083
    19  23       0.088895     23       0.000219
    20   0       0.196663      0       0.000191
    21   1       0.430047      1       0.000210
    22  27       1.110187     27       0.000254
    23  28       2.252374     28       0.000506
    24  30       5.447592     30       0.000260
    -------------------------------------------
                                       Table 1.
                                       


    
    N.B. The new implementation can indeed handle MASSIVE numbers:
    
    just type "exM (mers 21) (mers 22) 5"
    
  -}
   
  