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

{-

According to wikipedia, the rot13 encryption is specified by the following properties:

1. For each character of the string that is an element of the English alphabet (specified as the union
of a-z and A-Z) that character is replaced by the letter that is 13 places further along in the alphabet.
Wrapping is utilized for those letters whose corresponding trasformation exceeds beyond the total length 
of the alphabet.


Character:    a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
Replacement:  n o p q r s t u v w x y z a b c d e f g h i j k l m N O P Q R S T U V W X Y Z A B C D E F G H I J K L M

2. Those characters which are not alphabetical undergo no replacement and appear in the encrypted string
as they did in the original string.

Ex.

Input String:  "$H3ll0_W0rLd!!?"
Output String: "$U3yy0_J0eYq!!?"

3. For a string which has undergone encryption, if the corresponding output string is then encrypted again, the original
plaintext string is returned. This shows the rotational property of the encryption, or rather the identity property of the
string.

Ex.

rot13 "Hello!" == "Uryyb!" <-> rot13 "Uryyb!" == "Hello!"

-}

--Program

rot13 :: [Char] -> [Char]
rot13 s = map rotChar13 s

rotChar13 :: Char -> Char
rotChar13 c
    | c `elem` (['a'..'m'] ++ ['A'..'M']) = enumChar c 13
    | c `elem` (['n'..'z'] ++ ['N'..'Z']) = enumChar c (-13)
    | otherwise  = c

enumChar :: Char -> Int -> Char
enumChar c n = chr $ ord c + n


{- Properties to test according to specification -}

--Precondition:  Input is string
--Postcondition: The encryption of the encryption of the string is the original input string.
identityProp :: [Char] -> Bool
identityProp s = (rot13 $ rot13 s) == s


-- Precondition:  character is alphabetical
-- Postcondition: character is replaced by letter 13 positions further in alphabet in encryption
affectedProp :: Char -> Bool
affectedProp c = c `elem` (['a'..'z'] ++ ['A'..'Z']) --> ((rotChar13 c == enumChar c 13) || (rotChar13 c == enumChar c (-13)))


--Precondition:  character is non-alphabetical
--Postcondition: character remains unchanged in encryption
unaffectedProp :: Char -> Bool
unaffectedProp c = (not (c `elem` (['a'..'z'] ++ ['A'..'Z']))) --> (rotChar13 c == c)

{-

This was a very simple exercise. Once a full specification was written up determining which properties to test
was trivial.

Total Time Spent: 45 mins

-}