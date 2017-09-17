module Lab2 where
import Data.List
import Data.Char
import Test.QuickCheck

-- Specification from Wikipedia (https://en.wikipedia.org/wiki/ROT13):
-- ROT13 .. is a simple letter substitution cipher that replaces a letter with the letter 13 letters 
-- after it in the alphabet


rot13::String -> String
--rot13 xs = xs, pass first test
--rot13 xs = map (chr . (1+) . ord) xs
--rot13 xs = map (\x -> if isLetter x then (chr . (1+) . ord) x else x) xs
rot13 xs = map (\x -> if elem x (uc ++ lc) then encode x else x) xs
    where 
        uc = ['A'..'Z']
        lc = ['a'..'z']
        rotate n xs = (drop n xs) ++ (take n xs)
        letterSwaps = zip (uc ++ lc) ((rotate 13 uc) ++ (rotate 13 lc))
        encode l = snd $ head $ dropWhile ((l/=) . fst) letterSwaps

-- length remains the same 
-- If all letters every item should be changed
-- Uppercase remain upper case
-- Lowercase remains lower case
-- If no letters there will be no change
-- If mixture of letters and non letters change
-- If run on itself it should return original string

prop_sameLength xs = length xs == (length $ rot13 xs)

-- need to be more lenient for this to finish, so use quickCheckWith lenientArgs 
lenientArgs = (stdArgs {  maxDiscardRatio = 99 })
prop_allLettersChange xs = (all isLetter xs) ==> all (==True) $ zipWith (/=) xs (rot13 xs)

prop_allNonLettersStay xs = (all (not . isLetter) xs) ==> all (==True) $ zipWith (==) xs (rot13 xs)

prop_UppersRemainUpper xs = (all (isAsciiUpper) xs) ==> all isAsciiUpper $ rot13 xs 

prop_LowersRemainLower xs = (all (isAsciiLower) xs) ==> all isAsciiLower $ rot13 xs 

-- have to take into account that unicode alphas do not change
nonLatinAlpha x = (not $ isAsciiUpper x) && (not $ isAsciiLower x)
prop_NoAlphaRemainsSame xs = (all nonLatinAlpha xs) ==> xs == (rot13 xs)

aAndNonA xs = (elem True nonalphas) && (elem False nonalphas) 
    where 
        nonalphas = map nonLatinAlpha xs

prop_MixAlphaNonAlphaChanges xs = (aAndNonA xs) ==> xs /= (rot13 xs)

prop_MixNonAlphasAreSame xs = (aAndNonA xs) ==> (filter nonLatinAlpha xs) == (filter nonLatinAlpha (rot13 xs))

prop_MixAlphasAreAllDifferent xs =  (aAndNonA xs) ==> all (==False) $ zipWith (==) origA rot13A
    where
        origA = (filter (not . nonLatinAlpha) xs)
        rot13A = (filter (not . nonLatinAlpha) (rot13 xs))

prop_IsReflexive xs = xs == (rot13 $ rot13 xs)