module Exercise_2 where
  
import Lecture3
import Exercise_4
import Test.QuickCheck

{- 
   For notes on this exercise see:
   https://github.com/software-engineering-amsterdam/ST2017_WG_13/blob/master/Team/3/Exercise_2.md
-}

parseTesterInput :: [Form]
parseTesterInput = [

    form1,
    form2,
    form3,
    (Equiv form1 form1),
    (Impl form1 form2),
    (Neg ( Impl form1 form2))
  
  ]

prop_parsesOk :: Form -> Bool
prop_parsesOk form = [form] == parse ( show form )

parsesAllOk :: [Form] -> Bool
parsesAllOk =  all prop_parsesOk

test_parsesOk = verboseCheck prop_parsesOk
 