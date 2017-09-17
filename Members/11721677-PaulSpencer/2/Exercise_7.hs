module Lab2 where
import Data.List as L
import Data.Char
import System.Random
import Test.QuickCheck
import Data.Map as M
import Exercise_7_TestData

-- Deliverables: Haskell program, concise test report, indication of time spent.

-- Tests:
-- First that it works: 
validateIbansfromList::Property
validateIbansfromList = forAll validIbanIndexes validIbansValidatedProperty

-- And Now that it does not incorrectly work:
invalidIbansfromList::Property        
invalidIbansfromList = forAll indexAndOffset changedCheckNumIsInvalid

-- first, clean input then check the countrylength is valid against a map of the valid countryLengths.
-- Next calculate the checkSum by moving first 4 to end, converting letters to numbers and checking,
-- that the mod97 of the result
iban::String -> Bool
iban xs 
    | not goodCountryLength = False
    | otherwise             = ibanCheckSum 
        where 
            cleanxs = L.filter (/=' ') $ fmap toUpper xs
            countriesByLength =  maybe [] id $ M.lookup (length cleanxs) lengthAndCountry
            goodCountryLength = L.elem (take 2 cleanxs) countriesByLength
            ibanCheckSum = convertednumber `mod` 97 == 1
                where
                    firstShallBeLast = drop 4 cleanxs ++ take 4 cleanxs
                    gematria x = if isUpper x then (show . (flip ((-) . ord)) 55) x else [x]
                    convertednumber = read $ concat $ fmap gematria firstShallBeLast

lengthAndCountry::M.Map Int [String]
lengthAndCountry = M.fromList [
    (15, ["NO"]),
    (16, ["BE"]),
    (18, ["DK","FO","FI","GL","NL"]),
    (19, ["MK","SI"]),
    (20, ["AT","BA","EE","KZ","LT","LU","XK"]),
    (21, ["HR","LV","LI","CH"]),
    (22, ["BH","BG","CR","GB","GE","DE","IE","ME","RS","TL"]),
    (23, ["GI","IL","IQ","AE","TL"]),
    (24, ["AD","CZ","MD","PK","RO","SA","SK","ES","SE","TN","VG"]),
    (25, ["PT","ST"]),
    (26, ["IS","TR"]),
    (27, ["FR","GR","IT","MC","MR","SM"]),
    (28, ["AL","AZ","BY","CY","DO","GT","HU","LB","PL","SV"]),
    (29, ["BR","PS","QA","UA"]),
    (30, ["JO","KW","MU"]),
    (31, ["MT","SC"]),
    (32, ["LC"])]

--------------------

-- Check it validates correctly from a list pulled from the internet           
validIbansValidatedProperty k = iban $ validIbans !! k

-- generate indexes within the list to check
validIbanIndexes:: Gen Int
validIbanIndexes = choose (0, (length validIbans)-1)

----------------------

-- Check if manipulating the checknum of valid Ibans causes checksums to fail

-- generate list indexes for valid Ibans and numbers to add to the checknumber to invalidate it
indexAndOffset:: Gen (Int, Int)
indexAndOffset = (,) <$> choose (0, (length validIbans)-1) <*> choose (1,99)

-- get a valid Iban, extract the checknumber manipulate it to be invalid*, reinsert the manipulated
-- checknumber into the Iban and check that 
-- * note checknumber 0 1 or 2 + offset 97, or checknumber 97, 98 or 99 + offset 3 create valid checksums 
changedCheckNumIsInvalid :: (Int, Int) -> Bool
changedCheckNumIsInvalid (index, offset) = not $ iban invalidIbanCheckNum
    where
        validIban = validIbans !! index
        checknum = (read $ take 2 $ drop 2 validIban) :: Int
        invalidchecknum = if alsovalid then (checknum + offset + 1) else checknum + offset
        alsovalid = (offset == 97 && elem checknum [0,1,2]) || (offset == 3 && elem checknum [97,98,99])
        zeropad n = reverse $ take 2 $ reverse $ "00" ++ (show n)
        invalidIbanCheckNum = (take 2 validIban) ++ (zeropad invalidchecknum) ++ (drop 4 validIban)

