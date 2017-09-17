# Exercise 6

~ Total time spent = 45mins
### Implementing and testing ROT13 encoding ###

ROT13 is a single letter substitution cipher that is used in online forums for hiding spoilers.

First, give a specification of ROT13.

Next, give a simple implementation of ROT13.

Finally, turn the specification into a series of QuickCheck testable properties, and use these to test your implementation.

#### 1. Specification ####
According to wikipedia, the rot13 encryption is specified by the following properties:

1. For each character of the string that is an element of the English alphabet (specified as the union of a-z and A-Z) that character is replaced by the letter that is 13 places further along in the alphabet. Wrapping is utilized for those letters whose corresponding trasformation exceeds beyond the total length of the alphabet.

```
INPUT:   a b c d e f g h i j k l m n o p q r s t u v w x y z 
OUTPUT:  n o p q r s t u v w x y z a b c d e f g h i j k l m
```
```
INPUT:   A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
OUTPUT:  N O P Q R S T U V W X Y Z A B C D E F G H I J K L M
```
2. Those characters which are not alphabetical undergo no replacement and appear in the encrypted string as they did in the original string.

```
Input String:  "$H3ll0_W0rLd!!?"
Output String: "$U3yy0_J0eYq!!?"
```
3. For a string which has undergone encryption, if the corresponding output string is then encrypted again, the original plaintext string is returned. This shows the rotational property of the encryption, or rather the identity property of the string.

```
rot13 "Hello!" == "Uryyb!" <--> rot13 "Uryyb!" == "Hello!"
````

#### 2. Program ####

First we define `rot13` function which takes in an character list (or String) as input and outputs an encrypted character list. This encryption done via a `rotChar13` function which mutates each character of the passed character list.

```haskell
rot13 :: [Char] -> [Char]
rot13 s = map rotChar13 s
```

`rotChar13` simply checks if the passed character is within the first of second half of the alphabet.
* If it is in the first half, `rotChar13` passes the character to the `enumChar` function along with positive integer of 13, specifying the returned character should be 13 places from the right of the current passed character.
* If it is in the second half, `rotChar13` passes the character to the `enumChar` function along with negative integer of 13, specifying the returned character should be 13 places from the left of the current passed character. This shows the wrapping characteristic of `rot13`.
* If the character is not in either half of the alphabet, it is simply returned as no mutation is performed on non alphabetic characters.
```haskell
rotChar13 :: Char -> Char
rotChar13 c
    | c `elem` (['a'..'m'] ++ ['A'..'M']) = enumChar c 13
    | c `elem` (['n'..'z'] ++ ['N'..'Z']) = enumChar c (-13)
    | otherwise  = c
```

The `enumChar` function simply takes in the passed character, converts it to its unicode value and adds the passed integer value to the unicode value. This sum is then converted back to a character which is returned to the `rotChar13` function.

```haskell
enumChar :: Char -> Int -> Char
enumChar c n = chr $ ord c + n
```
#### 3. Testing ####

In order to test the implementation, 3 testable properties are created based on the specification.

1. The encryption of the encryption of the original string, is the original string itself.

**Precondition:**  Input is string

**Postcondition:** The encryption of the encryption of the string is the original input string.
```haskell
identityProp :: [Char] -> Bool
identityProp s = (rot13 $ rot13 s) == s
```
2. Only alphabetical characters are affected by the encryption. These are replaced by the character which is 13 places further along in the alphabet.

**Precondition:**  character is alphabetical

**Postcondition:** character is replaced by letter 13 positions further in alphabet
```haskell
affectedProp :: Char -> Bool
affectedProp c = 
        c `elem` (['a'..'z'] ++ ['A'..'Z']) --> 
        ((rotChar13 c == enumChar c 13) || (rotChar13 c == enumChar c (-13)))
```

3. Non-alphabetical characters are unaffected by the encrytion.

**Precondition:**  character is non-alphabetical

**Postcondition:** character remains unchanged in encryption
```haskell
unaffectedProp :: Char -> Bool
unaffectedProp c = 
        (not (c `elem` (['a'..'z'] ++ ['A'..'Z']))) --> (rotChar13 c == c)
```

QuickCheck on these properties passes all tests.
