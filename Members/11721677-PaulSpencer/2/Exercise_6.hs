module Lab2 where
import Data.List
import Data.Char
import Test.QuickCheck
import Data.Ascii
-- Implementing and testing ROT13 encoding

-- First, give a specification of ROT13.

-- Next, give a simple implementation of ROT13.

-- Finally, turn the specification into a series of QuickCheck testable properties, and use these to test 
-- your implementation.

-- Specification from Wikipedia (https://en.wikipedia.org/wiki/ROT13):
-- ROT13 .. is a simple letter substitution cipher that replaces a letter with the letter 13 letters 
-- after it in the alphabet


rot13::String -> String
--rot13 xs = xs, pass first test
--rot13 xs = map (chr . (1+) . ord) xs
--rot13 xs = map (\x -> if isLetter x then (chr . (1+) . ord) x else x) xs
rot13 xs = map (\x -> if isBasicLatin x then encode x else x) xs
    where 
        isBasicLatin x = (isAsciiLower x) || (isAsciiUpper x)
        rotate n xs = (drop n xs) ++ (take n xs)
        uc = ['A'..'Z']
        lc = ['a'..'z']
        letterSwaps = zip (uc ++ lc) ((rotate 1 uc) ++ (rotate 1 lc))
        encode l = snd $ head $ dropWhile ((l/=) . fst) letterSwaps

-- length remains the same 
-- If all letters every item should be changed
-- Uppercase remain upper case
-- Lowercase remains lower case
-- If no letters there will be no change
-- If mixture of letters and non letters then pattern of changed and unchanged should match pattern of letter 
--   and non-letter
-- If run on itself it should return original string

prop_sameLength xs = length xs == (length $ rot13 xs)

-- need to be more lenient for this to finish, so use quickCheckWith lenientArgs 
lenientArgs = (stdArgs {  maxDiscardRatio = 99 })
prop_allLettersChange xs = (all isLetter xs) ==> all (==True) $ zipWith (/=) xs (rot13 xs)

prop_allNonLettersStay xs = (all (not . isLetter) xs) ==> all (==True) $ zipWith (==) xs (rot13 xs)

prop_UppersRemainUpper xs = (all (isAsciiUpper) xs) ==> all isAsciiUpper $ rot13 xs 

