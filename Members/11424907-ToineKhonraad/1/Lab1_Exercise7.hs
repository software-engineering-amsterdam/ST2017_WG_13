module Lab1_Exercise7 where

import Data.Char

{-
  According to wikipedia:
  
  The formula verifies a number against its included check digit, 
  which is usually appended to a partial account number to generate 
  the full account number. This number must pass the following test:

  From the rightmost digit, which is the check digit, and moving left, 
  double the value of every second digit. 
    
  If the result of this doubling operation is greater than 9 
  (e.g., 8 × 2 = 16), then add the digits of the product 
  (e.g., 16: 1 + 6 = 7, 18: 1 + 8 = 9) or alternatively subtract 9 
  from the product (e.g., 16: 16 - 9 = 7, 18: 18 - 9 = 9).
  
  Take the sum of all the digits.
  
  If the total modulo 10 is equal to 0 (if the total ends in zero) 
  then the number is valid according to the Luhn formula; 
  else it is not valid.

-}
  

rtoleft :: Integer -> [Integer]
rtoleft n = [ toInteger (digitToInt c) | c <- reverse $ show n ] 

calc :: Integer -> [Integer]
calc n = [ if (2*digit> 9) then (2*digit-9) else digit  | digit <- (rtoleft n) ]

luhn :: Integer -> Bool
luhn n = sum (calc n) `mod` 10 == 0 

{-

  using https://www.cybersource.com/developers/getting_started/test_and_manage/best_practices/card_type_id/
-}


{-
  American Express
  Valid length: 15 digits. First digit must be a 3 and second digit must be a 4 or 7. 
-}
isAmericanExpress :: Integer -> Bool
isAmericanExpress n = ( luhn n ) && test ( show n ) where
                          test ( f:s:_) = f == '3' && (s == '4' || s == '7')

{-
  MasterCard
  Valid length: 16 digits.

  First digit must be 5 and second digit must be in the range 1 through 5 inclusive. 
  The range is 510000  through 559999.

  First digit must be 2 and second digit must be in the range 2 through 7 inclusive. 
  The range is 222100    through 272099.
-}
isMaster :: Integer -> Bool
isMaster n = ( luhn n ) && test ( show n ) where
  test s =
    length s == 16 &&
   head s == '5'  && (
   let ip = read ( drop 1 ( take 7 s ) ) in  
     (ip >= 510000 && ip <= 559999 ) || 
     (ip >= 222100 && ip <= 272099 )
   ) 

{-
  Visa
  Valid length: Up to 19 digits. First digit must be a 4. 
-}
isVisa :: Integer -> Bool
isVisa n = ( luhn n ) && (length (show n) < 19) && (head (show n) == '4')

{- Time taken: 1.5 hours -}