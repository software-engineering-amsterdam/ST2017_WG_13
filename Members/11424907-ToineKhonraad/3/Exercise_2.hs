module Exercise_2 where
  
import Lecture3

parseTesterInput :: [Form]
parseTesterInput = [

    form1,
    form2,
    form3,
    (Equiv form1 form1),
    (Impl form1 form2),
    (Neg ( Impl form1 form2))
  
  ]

parsesOk :: Form -> Bool
parsesOk form = [form] == parse ( show form )

parsesAllOk :: [Form] -> Bool
parsesAllOk =  all parsesOk 

{- to run the tests just type 
	
	parsesAllOk
	
	and expect "True"
-}