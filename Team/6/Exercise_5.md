# Exercise 5 
Total time spent ~3 hours.
most of this time was spent unsuccessfully trying to work out why the early Carmichael primes were not behaving as expected.

## Specifications of problem ##
> Use the list generated by the following function for a further test of Fermat's primality check.

````haskell
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]
````
>Read the entry on Carmichael numbers on Wikipedia to explain what you find. If necessary, consult other sources.

## The Code
The first thing we did was rename carmichael to chernick, as the formula presented was a version of J. Chernick 1939 theorem to find a subset of the Carmichael numbers:

```` haskell
chernick :: [Integer]
chernick = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]
````

Next, using `reportPrimeResult` and `primesTest` from Exercise 4 (in the members - paul folder), we created a test that showed that Fermat's little theorem almost always wrongly classifies these non primes as prime: 

```` haskell
chernickTest runs cnt = primesTest (reportPrimeResult runs) (take cnt chernick)
```` 

for example ther results with 20 random picks on the first 20 of the Chernick subset of Carmicheals numbers was as follows:

```` haskell
*Exercise_5> chernickTest 20 20
not prime: 56052361
not prime: 118901521
not prime: 216821881
not prime: 228842209
not prime: 1299963601
not prime: 2301745249
not prime: 9624742921
not prime: 11346205609
not prime: 13079177569
not prime: 27278026129
not prime: 65700513721
not prime: 71171308081
not prime: 100264053529
not prime: 168003672409
not prime: 172018713961
not prime: 173032371289
not prime: 464052305161
"total: 17 out of: 20"
````
Wikipedia, https://en.wikipedia.org/wiki/Carmichael_number, states that "A Carmichael number will pass a Fermat primality test to every base b relatively prime to the number, even though it is not actually prime". 20 random picks should lead to a very accurate selection, however, here we still see 85% of our numbers fooling the test.

We also tried it with the early Carmichael numbers (which we hard coded in the file Exercise_5_TestData.hs):

````haskell
carmichaelTest runs cnt = primesTest (reportPrimeResult runs) (take cnt firstCarmichaels)
````

when we try this on the first 100 Carmichael numbers with 30 random repeats, we get many fewer false positives, for example one of our test runs was as follows:
````haskell
*Exercise_5> carmichaelTest 30 100
not prime: 162401
not prime: 1909001
not prime: 3581761
not prime: 4335241
not prime: 5444489
not prime: 5481451
not prime: 6189121
not prime: 8134561
not prime: 9439201
"total: 9 out of: 100"
````

As Wikipedia says that our Carmichael numbers should always fool Fermats Little Theorum we have to assume either there is something wrong with our data, or algorithm. We however could not discover what this was.