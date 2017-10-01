# Exercise 7
~Total time spent: ~2 hrs 

### Automated Testing of Symmetric Closure and Transitive Closure Functions ###

Test the functions symClos and trClos from the previous exercises. Devise your own test method for this. Try to use random test generation. Define reasonable properties to test.

#### Properties of Symmetric Closure ####
Specification of the symmetric closure:
Given relation `R` and its symmetric closure `S`, for `S` to be a valid symmetric closure it must hold that:

`R` is subset of `S`
`S` is minimal
`S` is symmetric i.e. the union of `R` and its  inverse, `R-`

First an implementation of the subset property:
```haskell
isSubset :: Ord a => Rel a -> Rel a -> Bool
isSubset [] _ = True
isSubset ((x,y):xs) ys
        | ((x,y) `elem` ys) = isSubset xs ys
        | otherwise = False
```

Next we must specify the symmetric property, which tests if the union of `R` and its inverse are within `S`:
```haskell
isSym :: Ord a => Rel a -> Rel a -> Bool
isSym [] _ = True
isSym ((x,y):xs) ys 
            | (elem (x,y) ys) && (elem (y,x) ys) = isSym xs ys
            | otherwise = False
```

Finally a property which tests if `S` is minimal, in that no removal a an element from the relation still results in a valid symmetric closure of `R`.
```haskell
isMinSym :: Ord a => Rel a -> Rel a -> Bool
isMinSym r1 r2 = null $ filter (\(a,b) -> (b,a) `notElem` r1) (r2 \\ r1)
```

#### Properties of Transitive Closure ####
Specification of the symmetric closure:
Given relation `R` and its symmetric closure `S`, for `S` to be a valid transitive closure it must hold that:

`R` is subset of `S`
`S` is minimal
`S` is the minimal sum of all compositions `R` resulting in a transitive relation.

Subset property remains unchanged from the symmetric closure implementation.

Transitivty check property is borrowed from Exercise_6:
```haskell
transCheck :: Ord a => Rel a -> Bool
transCheck r = and [elem (x,z) r | (x,y) <- r, (w,z) <- r, y == w]
```

Finally a property which checks if `S` is minimal, in that no removal of an element of `S` results in a valid transitive closure of `R`:
```haskell
isMinTran :: Ord a => Rel a -> Rel a -> Bool
isMinTran r1 r2 = null $ filter (\pair -> transCheck (delete pair r2)) (r2 \\ r1)
```

#### Relation Generator ####
To perform automated testing of these properties, a relation generator is constructed which generates random valid relations and a test function modeled after quickCheck is implemented to test the properties over the randomly generated relations.

First the relation generator:
```haskell
relGen::(Random a, Show a) => Int -> IO (Rel a)
relGen n = do
    l <- getStdRandom (randomR (0,n))
    g <- getStdGen
return $ take l $ zip (randoms g) (randoms g)
```

quickCheck based relationTester:
```haskell
relTester ::(Random a, Show a) =>  Int -> Int -> (Rel a -> Bool) -> IO ()
relTester current total property = 
    if current == total then 
        print (show total ++ " tests passed")
    else do
        rel <- relGen 50
        if property rel then
            do 
                print ("pass on: " ++ show rel)
                relTester (current+1) total property
        else 
error ("failed test on: " ++ show rel)
```

#### Automated Testing ####
Once the from scratch relation generator and tester are implemented, we can test the combined properties of both the symmetric closure and transitive closure functions with a series of automated testing. We use both quickCheck's verboseCheck built-in function and the from scratch relTester function to test the validity of these two functions.

Automated testing for symmetric closure:
```haskell
prop_SymClos :: Ord a => Rel a -> Bool
prop_SymClos r = isSubset r s && isSym r s && isMinSym r s where s = symClos r
test_SymClos = relTester 1 100 (prop_SymClos :: Rel Int -> Bool)
test_SymClos' = verboseCheck (prop_SymClos :: Rel Int -> Bool)
```

Automated testing for transitive closure:
```haskell
prop_TrClos r = isSubset r s && isMinTran r s && transCheck s where s = trClos r
test_TrClos = relTester 1 100 (prop_TrClos :: Rel Char -> Bool)
test_TrClos' = verboseCheck (prop_TrClos :: Rel Char -> Bool)
```

#### Test Report ####

Both the symmetric and transitive closure functions pass all random test inputs.

Below are the first 10 inputs from the verboseCheck for each function, using Rel Int as the input type for both.

Output of `test_SymClos'`:
```
1. Passed: []

2. Passed: [(0,0)]

3. Passed: [(-1,2),(-2,1)]

4. Passed: [(-1,2)]

5. Passed: [(-1,4),(-3,3),(4,0),(-1,3)]

6. Passed: [(-1,-4),(-1,5),(-1,-5),(0,-1)]

7. Passed: [(6,3),(-1,-3),(-2,6),(3,2),(-5,4),(5,5)]

8. Passed: [(0,-6)]

9. Passed: [(8,4)]

10. Passed: [(-5,-7),(4,3),(9,1),(9,-6),(9,5),(1,-8),(-7,-4)]
```

Output of `test_TrClos'` (with Rel Int -> Bool specified):
```
1. Passed: []

2. Passed: [(0,1)]

3. Passed: [(0,1),(1,-1)]

4. Passed: [(-1,-1),(0,1),(3,2)]

5. Passed: [(1,0),(1,1),(2,-1),(1,-3)]

6. Passed: [(5,0),(-1,3)]

7. Passed: [(-6,-1),(0,4),(-3,3)]

8. Passed: []

9. Passed: [(-7,2),(-6,5),(5,6),(-7,-4),(3,2),(-2,4),(2,-4)]

10. Passed: [(3,-8),(-4,3),(-7,5),(0,4),(4,5),(3,1),(8,5)]
```
