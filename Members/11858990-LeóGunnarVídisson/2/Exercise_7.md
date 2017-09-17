# Exercise 7

### Implementing and testing IBAN validation ###

* 6 Hours
 
The International Bank Account Number (IBAN) was designed to facility international money transfer, to uniquely identify bank accounts worldwide. It is described here, including a procedure for validating IBAN codes. Write a function

iban :: String -> Bool
that implements this validation procedure.

Next, test your implementation using some suitable list of examples.
Note It is not enough to test only with correct examples. You should invent a way to test with incorrect examples also.
Can you automate the test process?
Deliverables: Haskell program, concise test report, indication of time spent.

#### 1. Writing the function ####

Precondtion(s)

  1) IBAN string may not be longer than 34 characters.

Postcondtion(s)

  1) The program should return True if the given string is a valid IBAN.
  
First I check if the country code matches that country's length. Here I remove all white spaces from the list.
And create a tuple and check if that tuble is an instance in my countryAndLength list.

```haskell
--removes the first two characters
input :: String -> String
input countryCode = take(2) countryCode

--Checks length and country code matches
validate ::String -> Bool
validate a = ( input a, length (filter (/=' ') a) ) `elem` countryAndLength 

```

Here I remove the first four characters, and then I append them to the list. I decided to remove all whitespaces again.
Some IBAN Numbers had a whitespace as a third element.

```haskell
--Removes the first 4 letters and appends them to a new list
remove4 :: String -> String
takeFirst4 :: String -> String
final :: String -> String
remove4 b = drop 4 (filter (/=' ')b)
takeFirst4 b = take 4 (filter (/=' ')b)
final b = remove4 (filter (/=' ')b) ++ takeFirst4 (filter (/=' ')b)
```

Now it is time to replace the characters with a numeric value. Here I use the ASCII value and deduct 55 from it.

```haskell
--Replace letters for numbers
replaceLetters :: [Char] -> [Char]
replaceLetters x = concat $ map convert x

convert :: Char -> [Char]
convert x 
    | x `elem` ['0'..'9'] = [x]
    | elem x ['A'..'Z'] = show (ord x - 55)
    | otherwise = error "FAIL"
```

Converting the String to an Integer.

```haskell
--Change string To a Integer
stringToInt :: String -> Integer
stringToInt a = read a :: Integer

```
Do the Modular Calculation.
```haskell
--Do the mod calculations
doMod :: Integer -> Bool
doMod a = a `mod`  97 == 1

```

This is a Helper Function. I found it more readable this way.
```haskell
-- iban Helper
help :: String -> Bool
help x = doMod(stringToInt(replaceLetters(filter (/=' ')(final x))))
```
IBAN 
```haskell
--IBAN 
iban :: String -> Bool
iban x = ((validate x) && (help x))
```



#### 3. Testing ####

I created a big dataset and tan that.

```haskell
  "valid IBAN"
  iban "IS57 0130 2602 9429 0202 9429 99"
  iban "AL47 2121 1009 0000 0002 3569 8741"
  iban "AD12 0001 2030 2003 5910 0100"
  iban "AT61 1904 3002 3457 3201"
  iban "AZ21 NABZ 0000 0000 1370 1000 1944"
  iban "BH67 BMAG 0000 1299 1234 56"
  iban "BE62 5100 0754 7061"
  iban "BA39 1290 0794 0102 8494"
  iban "BG80 BNBG 9661 1020 3456 78"
  iban "HR12 1001 0051 8630 0016 0"
  iban "CY17 0020 0128 0000 0012 0052 7600"
  iban "CZ65 0800 0000 1920 0014 5399"
  iban "DK50 0040 0440 1162 43"
  iban "EE38 2200 2210 2014 5685"
  iban "FO97 5432 0388 8999 44"
  iban "FI21 1234 5600 0007 85"
  iban "FR14 2004 1010 0505 0001 3M02 606"
  iban "GE29 NB00 0000 0101 9049 17"
  iban "DE89 3704 0044 0532 0130 00"
  iban "GI75 NWBK 0000 0000 7099 453"
  iban "GR16 0110 1250 0000 0001 2300 695"
  iban "GL56 0444 9876 5432 10"
  iban "HU42 1177 3016 1111 1018 0000 0000"
  iban "IS14 0159 2600 7654 5510 7303 39"
  iban "IE29 AIBK 9311 5212 3456 78"
  iban "IL62 0108 0000 0009 9999 999"
  iban "IT40 S054 2811 1010 0000 0123 456"
  iban "JO94 CBJO 0010 0000 0000 0131 0003 02"
  iban "KW81 CBKU 0000 0000 0000 1234 5601 01"
  iban "LV80 BANK 0000 4351 9500 1"
  iban "LB62 0999 0000 0001 0019 0122 9114"
  iban "LI21 0881 0000 2324 013A A"
  iban "LT12 1000 0111 0100 1000"
  iban "LU28 0019 4006 4475 0000"
  iban "MK072 5012 0000 0589 84"
  iban "MT84 MALT 0110 0001 2345 MTLC AST0 01S"
  iban "MU17 BOMM 0101 1010 3030 0200 000M UR"
  iban "MD24 AG00 0225 1000 1310 4168"
  iban "MC93 2005 2222 1001 1223 3M44 555"
  iban "ME25 5050 0001 2345 6789 51"
  iban "NL39 RABO 0300 0652 64"
  iban "NO93 8601 1117 947"
  iban "PK36 SCBL 0000 0011 2345 6702"
  iban "PL60 1020 1026 0000 0422 7020 1111"
  iban "PT50 0002 0123 1234 5678 9015 4"
  iban "QA58 DOHB 0000 1234 5678 90AB CDEF G"
  iban "RO49 AAAA 1B31 0075 9384 0000"
  iban "SM86 U032 2509 8000 0000 0270 100"
  iban "SA03 8000 0000 6080 1016 7519"
  iban "RS35 2600 0560 1001 6113 79"
  iban "SK31 1200 0000 1987 4263 7541"
  iban "SI56 1910 0000 0123 438"
  iban "ES80 2310 0001 1800 0001 2345"
  iban "SE35 5000 0000 0549 1000 0003"
  iban "CH93 0076 2011 6238 5295 7"
  iban "TN59 1000 6035 1835 9847 8831"
  iban "TR33 0006 1005 1978 6457 8413 26"
  iban "AE07 0331 2345 6789 0123 456"
  iban "GB29 RBOS 6016 1331 9268 19"
  iban "GB 29 NWBK 6016 1331 9268 19"
  
  "None Valid IBAN"
  iban "IS57 0130 2602 9429 1893 9429 99"
  iban "AL47 2121 1009 0000 3002 3569 8741"
  iban "AD12 0001 2030 2003 4910 0100"
  iban "AT61 1904 3002 3457 5501 2323"
  iban "AZ21 NABZ 0000 0000 1370 1320 1944"
  iban "BH67 BMAG 0000 1299 1234 5632"
  iban "BE62 5100 0754 7061 2344"
  iban "BA39 1290 0794 0102 8494 2222"
  iban "BG80 BNBG 9661 1020 3456 7823"
  iban "HR12 1001 0051 8630 0016 0232"
  iban "CC17 0020 0128 0000 0012 0052 7600"
  iban "DK65 0800 0000 1920 0014 5399"
  iban "CZ50 0040 0440 1162 43"
  iban "EE33 2200 2210 2014 5685"
  iban "FO27 5432 0388 8999 44"
  iban "FI21 4324 5600 0007 85"
  iban "FR14 1010 0505 0001 3M02 606"
  iban "GE29 NB18 0300 0101 9049 17"
  iban "DE89 3704 XDAE 0532 0130 00"
  iban "GI75 NWBK 0000 0000 7099"
  iban "GR16 0110 1250 0000 0001 2300"
  iban "GL56 0444 9876 5432 1001"
  iban "HU42 1177 3016 1121 1018 0000 0000"
  iban "IS14 0159 2600 7214 5510 7303 39"
  iban "IE34 AIBK 9311 5212 3456 78"
  iban "IL62 2308 0000 0009 9999 999"
  iban "IT40 2054 2811 1010 0000 0123 456"
  iban "JO94 123O 0010 0000 0000 0131 0003 02"
  iban "KW81 CBKU 0230 0000 0000 1234 5601 01"
  iban "LV80 BANK 2350 4351 9500 1"
  iban "LB62 0999 3450 0001 0019 0122 9114"
  iban "LI21 0881 0000 3234 013A A"
  iban "LT12 1000 0111 0100 2346"
  iban "LU28 0019 4006 4475 1111"
  iban "MK072 5012 0000 0589 8422"
  iban "MT84 MALT 0110 0001 2345 MEEE AST0 01S"
  iban "MU17 BOMM 0101 1010 3030 DDSA 000M UR"
  iban "MD24 AG00 0225 1000 1310 4DS8"
  iban "MC93 2005 2322 1001 1223 3M44 555"
  iban "ME25 5050 0022 2345 6789 51"
  iban "NL39 RABO 0310 0652 64"
  iban "NO93 8601 1117 917"
  iban "PK36 SCBL 0000 0023 2345 6702"
  iban "PL60 1020 1026 0320 0422 7020 1111"
  iban "PT50 0002 0123 1124 5678 9015 4"
  iban "QA58 DOHB 0000 4321 5678 90AB CDEF G"
  iban "RO49 AAAA 1B31 0022 9384 0000"
  iban "SM86 U032 2509 8543 0000 0270 100"
  iban "SA03 8000 9999 6080 1016 7519"
  iban "RS35 2600 HALL 1001 6113 79"
  iban "SK31 1200 SUMM 1987 4263 7541"
  iban "SI56 1910 0000 EGTU 438"
  iban "ES80 2310 0001 HVAR 0001 2345"
  iban "SE35 5000 0000 SARA 1000 0003"
  iban "CH93 0076 LEOG 6238 5295 7"
  iban "TN59 1000 6935 1835 9847 8831"
  iban "TR33 0006 1005 1978 6457 8413"
  iban "AE07 0331 2345 6789 0123"
  
  ```
#### 4. Automation ####

I'm not sure If that is possible.
Since IBANs are not all of the same lengths

