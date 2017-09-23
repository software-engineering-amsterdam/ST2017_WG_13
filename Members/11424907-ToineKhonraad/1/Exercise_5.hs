module Exercise_5 where
  
sieve::[Integer] -> [Integer]
sieve (p:xs)  = p: sieve [x | x <- xs, x `mod` p /=0]

primes::[Integer]
primes = sieve [2..]

isPrime :: Integer -> Bool 
isPrime n = all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]
  

sum_n_elements :: Integer -> [Integer] -> Integer
sum_n_elements n xs = sum $ take (fromIntegral n) xs 

findFirst :: Integer -> [Integer] -> Integer
findFirst n pt@(x:xs) 
    | isPrime (sum_n_elements n pt) = sum_n_elements n pt
    | otherwise = findFirst n xs 

{-

  running "findFirst 101 primes"
  
  then gives us the answer 37447
                           =====
  
  Testing this would effectively mean testing that functions
  like "take" and "sum" act as expected.
  
  Yet, as to gain more confidence we could come up with a
  list of pairs like so:
-}

find_pairs_till :: Integer -> [ (Integer, Integer ) ]
find_pairs_till n = [ ( k, findFirst k primes ) | k <- takeWhile (<=n) primes ]

{-

  And lift this to a printable report:
-}

tabularReport :: Integer -> IO ()
tabularReport n = mapM_ print $ find_pairs_till n

{-
  running "tabularReport 101" then gives us:
  
    (2,5)
    (3,23)
    (5,53)
    (7,197)
    (11,233)
    (13,691)
    (17,499)
    (19,857)
    (23,1151)
    (29,2099)
    (31,2399)
    (37,2909)
    (41,3821)
    (43,4217)
    (47,5107)
    (53,6079)
    (59,10091)
    (61,8273)
    (67,12281)
    (71,11597)
    (73,12713)
    (79,15527)
    (83,22741)
    (89,26041)
    (97,25759)
    (101,37447)

-}

{- Time taken: 2.5 hours -}