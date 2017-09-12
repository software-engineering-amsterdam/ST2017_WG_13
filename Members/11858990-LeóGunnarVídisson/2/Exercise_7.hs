-- Implementing and testing IBAN validation

-- The International Bank Account Number (IBAN) was designed to facility international money transfer, 
-- to uniquely identify bank accounts worldwide. It is described here, including a procedure for validating 
-- IBAN codes. Write a function

-- iban :: String -> Bool
-- that implements this validation procedure.

-- Next, test your implementation using some suitable list of examples.

-- Note It is not enough to test only with correct examples. You should invent a way to test with incorrect 
-- examples also.

-- Can you automate the test process?

-- Deliverables: Haskell program, concise test report, indication of time spent.

module Iban where


--Tuples
countryAndLength = [("AL", 28), ("AD", 24), ("AT", 20), ("AZ", 28), ("BH", 22),("BE", 16), ("BA", 20), ("BG", 22), ("HR", 21), ("CY", 28), ("CZ", 24), ("DK", 18), ("EE", 20), ("FO", 18), ("FI", 18), ("FR", 27), ("GE", 22), ("DE", 22), ("GI", 23), ("GR", 27), ("GL", 18), ("HU", 28), ("IS", 26), ("IE", 22), ("IL", 23), ("IT", 27), ("JO", 30), ("KW", 30), ("LV", 21), ("LB", 28), ("LI", 21), ("LT", 20), ("LU", 20), ("MK", 19), ("MT", 31), ("MU", 30), ("MD", 24), ("MC", 27), ("ME", 22), ("NL", 18), ("NO", 15), ("PK", 24), ("PL", 28), ("PT", 25), ("QA", 29), ("RO", 24), ("SM", 27), ("SA", 24), ("RS", 22), ("SK", 24), ("SI", 19), ("ES", 24), ("SE", 24), ("CH", 21), ("TN", 24), ("TR", 26), ("AE", 23), ("GB", 22)] 



input :: String -> String
input countryCode = take(2) countryCode


validate ::String -> Bool
validate a = ( input a, length (filter (/=' ') a) ) `elem` countryAndLength 



{-
iban :: String -> Bool
iban 
-}
