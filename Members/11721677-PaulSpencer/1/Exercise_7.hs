module Lab1 where 
import Data.List
import Data.Char

luhn :: Integer -> Bool
luhn cc = 0 == (luhnsum cc) `mod` 10
    where
        luhnsum n = ld $ map digitToInt $ reverse $ show n
        ld [] = 0
        ld [x] = x
        ld (x:y:zs) = x + ((2*y) `div` 10) + ((2*y) `mod` 10) + ld zs
        
isAmericanExpress:: Integer -> Bool
isAmericanExpress n = rightLength && goodPrefix && luhn n
    where 
        cs = show n
        rightLength = 0 < length cs && length cs <= 15
        prefix = take 2 cs
        goodPrefix = prefix == "34" || prefix == "37" 

isMaster:: Integer -> Bool
isMaster n = rightLength && goodPrefix && luhn n
    where
        cs = show n
        rightLength = 5 < length cs && length cs <= 19
        prefix = read $ take 6 cs
        goodPrefix = (prefix >= 510000 && 559999 >= prefix) || (prefix >= 222100 && 272099 >= prefix)         

isVisa :: Integer -> Bool
isVisa n = rightLength && goodPrefix && luhn n
    where 
        cs = show n
        rightLength = 0 < length cs && length cs <= 19
        goodPrefix = head cs == '4'

{-
easy peasy, minutes on this (more time looking up than doing)
was particularly proud of my luhn implementation
-}
