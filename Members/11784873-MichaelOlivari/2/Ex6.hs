module Lab1 where
import Data.List
import Data.Char
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Q6

enumChar :: Char -> Int -> Char
enumChar c n = chr $ ord c + n

rot13 :: [Char] -> [Char]
rot13 s = map rotChar13 s

rotChar13 :: Char -> Char
rotChar13 c
    | c `elem` (['a'..'m'] ++ ['A'..'M']) = enumChar c 13
    | c `elem` (['n'..'z'] ++ ['N'..'Z']) = enumChar c (-13)
    | otherwise  = c

