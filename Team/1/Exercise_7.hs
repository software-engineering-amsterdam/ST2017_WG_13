module Lab1 where 
import Data.List
import Test.QuickCheck
import Data.Char

-- inspired by genius!
luhn :: Integer -> Bool
luhn cc = 0 == (luhnsum cc) `mod` 10
    where
        luhnsum n = ld $ map digitToInt $ reverse $ show n
        ld [] = 0
        ld [x] = x
        ld (x:y:zs) = x + ((2*y) `div` 10) + ((2*y) `mod` 10) + ld zs

data CCVendor = Visa | AmericanExpress | MasterCard

isAmericanExpress:: Integer -> Bool
isAmericanExpress n = goodCard AmericanExpress n

isMasterCard::Integer -> Bool
isMasterCard n = goodCard MasterCard n

isVisaCard::Integer -> Bool
isVisaCard n = goodCard Visa n

goodCard::CCVendor -> Integer -> Bool
goodCard cc n = all (==True) [rightLength cc ccnr, goodPrefix cc ccnr, luhn n]
    where ccnr = show n
    
rightLength::CCVendor -> String -> Bool
rightLength Visa ccnr = 0 < length ccnr && length ccnr <= 19
rightLength MasterCard ccnr =  5 < length ccnr && length ccnr <= 19
rightLength AmericanExpress ccnr = 0 < length ccnr && length ccnr <= 15

goodPrefix::CCVendor -> String -> Bool
goodPrefix Visa ccnr = head ccnr == '4'
goodPrefix MasterCard ccnr = let pf = read $ take 6 ccnr in elem pf $ [510000..559999]++[222100..272099]
goodPrefix AmericanExpress ccnr = let pf = take 2 ccnr in pf == "34" || pf == "37" 



{-
(tests would just be to use valid cc numbers and run them past the various CC validators)
This was relatively simple (particularly if one had seen the problem in different languages)
-}