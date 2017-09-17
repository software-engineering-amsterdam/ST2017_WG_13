module Iban where
import Data.List.Utils
import Data.Char

--Tuples for country code and length
countryAndLength = [("AL", 28), ("AD", 24), ("AT", 20), ("AZ", 28), ("BH", 22),("BE", 16),
 ("BA", 20), ("BG", 22), ("HR", 21), ("CY", 28), ("CZ", 24), ("DK", 18), ("EE", 20), 
 ("FO", 18), ("FI", 18), ("FR", 27), ("GE", 22), ("DE", 22), ("GI", 23), ("GR", 27), 
 ("GL", 18), ("HU", 28), ("IS", 26), ("IE", 22), ("IL", 23), ("IT", 27), ("JO", 30), 
 ("KW", 30), ("LV", 21), ("LB", 28), ("LI", 21), ("LT", 20), ("LU", 20), ("MK", 19), 
 ("MT", 31), ("MU", 30), ("MD", 24), ("MC", 27), ("ME", 22), ("NL", 18), ("NO", 15), 
 ("PK", 24), ("PL", 28), ("PT", 25), ("QA", 29), ("RO", 24), ("SM", 27), ("SA", 24), 
 ("RS", 22), ("SK", 24), ("SI", 19), ("ES", 24), ("SE", 24), ("CH", 21), ("TN", 24), 
 ("TR", 26), ("AE", 23), ("GB", 22)] 


input :: String -> String
input countryCode = take(2) countryCode

--Checks length and countrycode matches
validate ::String -> Bool
validate a = ( input a, length (filter (/=' ') a) ) `elem` countryAndLength 


--Removes the first 4 letters and appends them to a new list
remove4 :: String -> String
takeFirst4 :: String -> String
final :: String -> String
remove4 b = drop 4 (filter (/=' ')b)
takeFirst4 b = take 4 (filter (/=' ')b)
final b = remove4 (filter (/=' ')b) ++ takeFirst4 (filter (/=' ')b)

--Replace letters for numbers
replaceLetters :: [Char] -> [Char]
replaceLetters x = concat $ map convert x

convert :: Char -> [Char]
convert x 
    | x `elem` ['0'..'9'] = [x]
    | elem x ['A'..'Z'] = show (ord x - 55)
    | otherwise = error "FAIL"

--Change string To a Integer
stringToInt :: String -> Integer
stringToInt a = read a :: Integer

--Do the mod calculations
doMod :: Integer -> Bool
doMod a = a `mod`  97 == 1

-- iban Helper
help :: String -> Bool
help x = doMod(stringToInt(replaceLetters(filter (/=' ')(final x))))

--Iban 
iban :: String -> Bool
iban x = ((validate x) && (help x))