# Exercise 6 
Total time spent ~2 hours.

## Specifications of problem ##
Exercise 6 comes in 2 parts: 
Firstly (which appears to be a follow-up to Exercise 5):
>Use the list from the previous exercise to test the Miller-Rabin primality check. What do you find?

Secondly:
>You can use the Miller-Rabin primality check to discover some large Mersenne primes. The recipe: take a prime p, and use the Miller-Rabin algorithm to check whether 2^p−1 is also prime. Find information about Mersenne primes on internet and check whether the numbers that you found are genuine Mersenne primes. Report on your findings.

## Part 1 Code: 
Here we adapt our tester to take in the Miller-Rabin prime checker `primeMR` from the lecture code:

````haskell
reportMRPrimeResult::Int -> Integer -> IO Int
reportMRPrimeResult c x = do  
    b <- primeMR c x
    if b then putStrLn $ "not prime: " ++ (show x) else putStr ""
    return (if b then 1 else 0)

chernickMRTest runs cnt = primesTest (reportMRPrimeResult runs) (take cnt chernick)
````

This results in a much more succesfull result set, we tried a number of different versions at lower and lower random runs. With 2 random runs we could get about a 1% false positive rate with 3 we got 0.1% false positive, with 4 random runs we did not get a single false positive:

````haskell
*Exercise_6> chernickMRTest 2 1000
not prime: 118974229155289
not prime: 742403294138881
not prime: 883519506462529
not prime: 2887649314391089
not prime: 149230042617461401
not prime: 187268560492386601
not prime: 201714130626423769
not prime: 991874518536877849
not prime: 1065859697776080169
not prime: 1066679002455792841
"total: 10 out of: 1000"
*Exercise_6> chernickMRTest 3 2000
not prime: 95102641231906969
*Exercise_6> chernickMRTest 2 1000
not prime: 118974229155289
not prime: 742403294138881
not prime: 883519506462529
not prime: 2887649314391089
not prime: 149230042617461401
not prime: 187268560492386601
not prime: 201714130626423769
not prime: 991874518536877849
not prime: 1065859697776080169
not prime: 1066679002455792841
"total: 10 out of: 1000"
*Exercise_6> chernickMRTest 3 2000
not prime: 95102641231906969
not prime: 21701282292084917161
"total: 2 out of: 2000"
````


## Part 2 Code: 
Part 2 was testing Mersene exponents

```` haskell
printMer::Integer -> IO Integer
printMer x = do  
    b <- primeMR 10 ((2^x)-1)
    if b then putStrLn $ (show x) ++ " = (2^" ++ (show x) ++ "-1)" else putStr ""
    return (if b then 1 else 0)

test_mersprimes::[Integer] -> IO String
test_mersprimes ts =  return ("total " ++) <*> (return show <*> (mp ts))
    where 
        mp = foldr (\x b -> pure (+) <*> (printMer x) <*> b) (return (0))       
````
we ran this for a short time and received the following:
````haskell
*Exercise_6> test_mersprimes $ take 2000 $ filter prime [2..]
2 = (2^2-1)
3 = (2^3-1)
5 = (2^5-1)
7 = (2^7-1)
13 = (2^13-1)
17 = (2^17-1)
19 = (2^19-1)
31 = (2^31-1)
61 = (2^61-1)
89 = (2^89-1)
107 = (2^107-1)
127 = (2^127-1)
521 = (2^521-1)
607 = (2^607-1)
1279 = (2^1279-1)
2203 = (2^2203-1)
2281 = (2^2281-1)
3217 = (2^3217-1)
4253 = (2^4253-1)
4423 = (2^4423-1)
9689 = (2^9689-1)
9941 = (2^9941-1)
11213 = (2^11213-1)
"total 23"
````

The On-Line Encyclopedia of Integer Sequences https://oeis.org/ states that the first 23 Mersene exponents were:
````
1		2
2		3
3		5
4		7
5		13
6		17
7		19
8		31
9		61
10		89
11		107
12		127
13		521
14		607
15		1279
16		2203
17		2281
18		3217
19		4253
20		4423
21		9689
22		9941
23		11213
````

These match our results