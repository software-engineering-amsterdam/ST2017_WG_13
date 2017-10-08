# Exercise 4 #
Total time spent 4 hours as follow:
mostly rewriting code so I could try and understand it and then throwing that code away.

## Specifications of problem ##
> The goal of this exercise is to extend the Sudoku program described in the lecture of this week with functions that can also handle Sudokus of a special kind: the Sudokus that appear in the Dutch evening newspaper NRC-Handelsblad each week (designed by Peter Ritmeester, from Oct 8, 2005 onward). These NRC Sudokus are special in that they have to satisfy a few extra constraints: in addition to the usual Sudoku constraints, each of the 3×33×3 subgrids with left-top corner (2,2), (2,6), (6,2), and (6,6) should also yield a surjective function. The above figure gives an example (this is the NRC sudoku that appeared Saturday Nov 26, 2005).

## Solution to the puzzel
It was required to solve this problem:

+---------+---------+---------+
|         | 3       |         |
|   +-----|--+   +--|-----+   |
|   |     | 7|   |  | 3   |   |
| 2 |     |  |   |  |     | 8 |
+---------+---------+---------+
|   |   6 |  |   |5 |     |   |
|   +-----|--+   +--|-----+   |
|    9  1 | 6       |         |
|   +-----|--+   +--|-----+   |
| 3 |     |  | 7 |1 | 2   |   |
+---------+---------+---------+
|   |     |  |   |  |    3| 1 |
|   |8    |  | 4 |  |     |   |
|   +-----|--+   +--|-----+   |
|       2 |         |         |
+---------+---------+---------+

the solution is:

+----------+-----------+----------+
| 4   7  8 | 3   9   2 | 6  1   5 |
|   +------|---+   +---|------+   |
| 6 | 1  9 | 7 | 5 | 8 | 3  2 | 4 |
|   |      |   |   |   |      |   |
| 2 | 3  5 | 4 | 1 | 6 | 9  7 | 8 |
+---|------+---|---|---+------|---+
| 7 | 2  6 | 8 | 3 | 5 | 1  4 | 9 |
|   +------|---+   +---|------+   |
| 8   9  1 | 6   2   4 | 7  5   3 |
|   +------|---+   +---|------+   |
| 3 | 5  4 | 9 | 7 | 1 | 2  8 | 6 |
+---|------+---|---|---+------|---+
| 5 | 6  7 | 2 | 8 | 9 | 4  3 | 1 |
|   |      |   |   |   |      |   |
| 9 | 8  3 | 1 | 4 | 7 | 5  6 | 2 |
|   +------|---+   +---|------+   |
| 1   4  2 | 5   6   3 | 8  9   7 |
+----------+-----------+----------+

## Timing

This is relevant for Q2, when I set my ghci settings with `:set +s` and ran this 1000 times with the following code we got the following time/size:  (5.11 secs, 508,649,672 bytes)

```` haskell
solveNrc = solveAndShow exampleNrc

redoAction n action
  | n <= 0    = return () 
  | otherwise = do 
    action  
    redoAction (n-1) action

nrc1000 = redoAction 1000 solveNrc
````

## Program ##

### nrc updates

first we added new block to describe the nrc blocks ang generate locations:

```` haskell
stdblocks,nrcblocks::[[Int]]
stdblocks =[[1..3],[4..6],[7..9]]
nrcblocks = [[2..4],[6..8]]

nrcgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- nrcblocks, cs <- nrcblocks ]
```

Next update finding grid locations and values for the nrc grid:

````haskell
nrclocs4loc:: Location -> [Location]
nrclocs4loc loc = sglocs4loc loc nrcgridlocs

nrcGrid :: Sudoku -> Location -> [Value]
nrcGrid s loc = [s loc' | loc' <- nrclocs4loc loc]
````

next Find remaining available numbers in nrc grid
````haskell
freeInNrcgrid :: Sudoku -> Location -> [Value]
freeInNrcgrid s (r,c) = freeInSeq (nrcGrid s (r,c))
````

Next update the consistency checker
````haskell
nrcgridInjective :: Sudoku -> Location -> Bool
nrcgridInjective s loc = injective $ gone $ nrcGrid s loc

consistent :: Sudoku -> Bool
consistent s = and $ consistentRow ++ consistentCol ++ consistentSub ++ consistentNrc
  where 
    consistentRow = isInjective rowInjective rowlocs
    consistentCol = isInjective colInjective collocs
    consistentSub = isInjective subgridInjective subgridlocs
    consistentNrc = isInjective nrcgridInjective nrcgridlocs
    isInjective inj locs =  [inj s loc | loc <- map head locs ]
````

Finally, update the pruner to check

````haskell
sameNrcblock :: Location -> Location -> Bool
sameNrcblock lx ly = (not. null $ nrclocs4loc lx) && (nrclocs4loc lx == nrclocs4loc ly)

prune :: (Location,Value) -> [Constraint] -> [Constraint]
prune _ [] = []
prune (lx@(r,c),v) ((ly@(x,y),vs):rest)
  | connected = trim : next
  | otherwise = (ly,vs) : next
  where
    connected = (r == x) || (c == y) || sameblock lx ly || sameNrcblock lx ly
    trim = (ly,vs\\[v])
    next = prune (lx,v) rest
````


### Unnecessary changes
For readability, naming and formatting changes were added.  We also converged on Location that is similar to Position in Q2.  code that was not necessary for this question was removed.  location helpers were added:

````haskell
type Location = (Row, Column)

alllocs::[Location]
alllocs = [(r,c) | r <- positions, c <- positions]

rowlocs, collocs,subgridlocs,nrcgridlocs::[[Location]]
rowlocs = [[(r,c) | c <- positions] | r <- positions]
collocs = [[(r,c) | r <- positions] | c <- positions]
subgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- stdblocks, cs <- stdblocks ]
nrcgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- nrcblocks, cs <- nrcblocks ]
````

completely unnecessarily I update the showGrid, to make it simpler to use for me. (this can be ignored)

````haskell
type DrawLoc = (Int, Int)
cs = [0..35]
rs = [0..18]

vlocs = [(r,c) | r <- rs, c <- [2,6,9,13,17,21,25,28,32], odd r]
mainGridHLines = [0,6,12,18]
mainGridVLines = [0,11,23,34]
nrcGridHLines = [[2,8],[10,16]]
nrcGridVLines = [[4,15],[19,30]]

corners,stdcorners,nrccorners::[DrawLoc]
corners = stdcorners ++ nrccorners
stdcorners = [(r,c) | r <- mainGridHLines, c <- mainGridVLines]
nrccorners = [(r,c) | r <- concat nrcGridHLines, c <- concat nrcGridVLines]

vlines,stdvlines,nrcvlines::[DrawLoc]
vlines = stdvlines ++ nrcvlines
stdvlines = [(r,c) | r <- rs, c <- mainGridVLines]
nrcvlines = [(r,c) | r <- [2..8]++[10..16], c <- concat nrcGridVLines]

hlines,stdhlines,nrchlines::[DrawLoc]
hlines = stdhlines ++ nrchlines
stdhlines = [(r,c) | r <- mainGridHLines, c <- cs]
nrchlines = [(r,c) | r <- concat nrcGridHLines, c <- [4..15]++[19..30]]

drawingPositions::[DrawLoc]
drawingPositions = [(r, c) | r <- rs, c <- cs]
    
showGrid::Grid -> IO ()
showGrid vs = showGrid' vs drawingPositions 
  where 
    showGrid' _ [] = putStrLn "" 
    showGrid' vs (loc@(r,c) : ps)
      | c == last cs       = colEnd
      | loc `elem` vlocs   = value'
      | loc `elem` corners = corner
      | loc `elem` vlines  = vline
      | loc `elem` hlines  = hline
      | otherwise          = empty
        where
          value' = next valueAtLocation
          valueAtLocation = maybe ' ' head  $ lookup loc $ zip vlocs (map showVal (concat vs)) 
          colEnd = next '\n'
          corner = next '+'
          vline = next '|'
          hline = next '-'
          empty = next ' '
          next c = do putChar c ; showGrid' vs ps

````

#### Testing Random Generator
In order to use our random `Set Int` generator we modified the tester from Lesson 2, to handle both the type and the generator:

````haskell
setIntTester :: Int -> Int -> (Set Int -> Set Int) -> (Set Int -> Set Int -> Bool) -> IO ()
setIntTester current total functionToTest isValid = 
    if current == total then 
        print (show total ++ " tests passed")
    else do
        si <- siGen 20
        if isValid si (functionToTest si) then
            do 
   print ("pass on: " ++ show si)
   setIntTester (current+1) total functionToTest isValid
        else 
            error ("failed test on: " ++ show si)
```` 

Then we made a property, `prop_areEqual` - to check for equality, and a function, `unionEmpty` - to union a set with an empty set, to be used in the tester, `test_areEqual`:

````haskell
prop_areEqual::Set Int -> Set Int -> Bool
prop_areEqual x y = x == y

unionEmpty::Set Int -> Set Int
unionEmpty si = unionSet si emptySet

test_areEqual::IO()
test_areEqual = setIntTester 1 100 unionEmpty prop_areEqual
````

Whilst this does not show that the values are random it does show that it at least produces valid values.

### Test (List Int) with QuickCheck
It was suggested that we could use QuickCheck to generate the input values to test Set Int. To do this we allow QuickCheck to generate the arbitrary list of `Int` and then apply `list2set` to those values.

````haskell
prop_setWithList::[Int] -> Bool
prop_setWithList xs = (list2set xs) == (unionEmpty $ list2set xs) 
test_setWithList = verboseCheck prop_setWithList    
````

Some example passing tests:
Passed:
[]

Passed:
[-2]

Passed:
[2,-2,4,-4]

Passed:
[-72,-6,27,-15,7,20,15,74,73,-33,-67,80]

Passed:
[18,63,76,-81,-53,-92,-17,44,68,-61,60,43,94,-80,55,7,-38,-15,80,56,66,54,-91,-24,-41,40,-12,-89,43,39,-66,73,91,-93,-74,-47,-7,28,82,-9,-90,-38,-11,32,-10,22,70,39,-72,-52,6,63,-67,-44,-47,59,-14,15,-98,-29,-18,-84,-46,-45,-32,-78,90,-55,-41,52,41,7,-57,-31,1,-55]

+++ OK, passed 100 tests.

However, on the basis that we thought that this question could not possibly be that easy, we rejected this suggestion and include this code only for completeness.

### Generate (Set a) with QuickCheck
For QuickCheck to generate tests for a type, that type has to implement the class `Arbitrary`, which has the `arbitrary` function which returns a generator, `Gen a`, that will create an arbitrary value of the type. 

First off we created an `Arbitrary` instance of the type `Set a`. To do this we allowed quickcheck to pick the maximum size of the input list by using the `sized` function.  Next we applied the `sized` function to a lambda that did the following:
1. Took in a maximun list length `n`;
1. Generated an list of arbitrary values using `arbitraryListGen` which:
   1. Using `choose`, picks an arbitrary number between 0 and the parameter;
   1. the number is bound to `vector` that will create a list of the lenght of the bound number of arbitrary values.
1. Applies `list2set` to this list to create a value of type `Set a`;
1. Wraps the value in a `Gen (Set a)` monad using return.

````haskell
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = sized (\n -> do
        lst <- arbitraryListGen n
        return $ list2set lst)

arbitraryListGen::(Ord a, Arbitrary a) => Int -> Gen [a]
arbitraryListGen n = choose (0,n) >>= vector
````

#### Testing (Set a) with QuickCheck
Now that we have implemented `Arbitrary` for `Set a`, we can use QuickCheck to generate a `Set Int`, on the property as follows:

````haskell
prop_arbitrarySetA::Set Int -> Bool
prop_arbitrarySetA si = si == (unionEmpty si)
test_arbitrarySetA = verboseCheck prop_arbitrarySetA
````
Some Example results

Passed:
{}

Passed:
{-1}

Passed:
{-2}

Passed:
{-91,-82,-75,-74,-66,-65,-63,-61,-56,-49,-48,-46,-42,-37,-33,-25,-18,-16,-8,-7,-5,11,13,14,18,21,24,25,27,37,44,50,54,55,57,63,64,71,73,77,78,82,84,86,89,91}

Passed:
{-48,-45,-29,-19,7,32,47,67}

+++ OK, passed 100 tests.

### Generate (Set Int) with QuickCheck
Implementing `Arbirary` for `Set a` has two major disadvantages. Firstly, and most importantly to this exercise, it could be argued that this is not what was asked for. The question asks us to use QuickCheck to test this datatype, and to an extent we are testing `Set a` and not `Set Int`. Secondly, and more importantly for us, by only testing `Set a` we cede control of the generation of data points to that that can be generated for data types that implement `Ord`.  This means that the test results on `Set a`, which are in turn created by the default implementation of `Arbitrary Int` when generated from the `vector` function.  This causes, for some reason, only numbers between -99 and +99. We wanted to test more numbers. As the `a` in `Set a` refers to types that implement `Ord` we would only be to add a generator that created items that were `Ord` in the `arbitary` function of `Arbitrary (Set a)`. If we want to create our own generator that is `Int` specific, we will have to implement `Arbitrary (Set Int)`.

It is not posible to create an `Arbitrary (Set Int)` implementation without using `{-# LANGUAGE FlexibleInstances #-}` GHC language extension.  As this would affect portablity of the code to other compilers, we chose to instead use `newtype` to wrap `Set Int`:

````haskell 
newtype SetInt = SetInt { unSetInt :: Set Int } deriving (Eq, Show)
````

Now we could go ahead and specifically test `Set Int` by implementing `Arbitrary` on our new type `SetInt`.  This has the extra overhead of packing and unpacking the type, however it gives us the advantage of being able to add our own random integer generation `intListGen`.  We decided that the numbers created whilst useful were not satisfactory, so we wanted to generate larger numbers.  To do this we use `choose (minBound, maxBound)`, to pick any valid integer, and then recursively build a list. In our instance of `Arbitrary` we use `oneof` to call either arbitraryListGen or arbitraryListGen. 

````haskell
instance Arbitrary SetInt where 
    arbitrary = sized (\n -> do
        lst <- oneof [(intListGen n), (arbitraryListGen n)]
        return $ SetInt $ list2set lst)

intListGen::Int -> Gen [Int]
intListGen n = do
    r <- choose (minBound, maxBound)
    rs <- intListGen (n-1)
    return $ r : (take (n-1) rs)
```` 

#### Testing Generate (Set Int) with QuickCheck
Wrapping `Set Int` makes testing a more cumbersome. To do the same test as above, i.e. that a union with an empty test is the same, means we have to write a new function for `unionSet'` and `unionEmpty'` to handle the new type `SetInt`:

````haskell 
unionSet'::SetInt -> SetInt -> SetInt 
unionSet' si1 si2 =  SetInt $ unionSet (unSetInt si1) (unSetInt si2)

unionEmpty'::SetInt -> SetInt
unionEmpty' si = unionSet' si (SetInt emptySet)
````
With these now in place we can now test as follows:

````haskell
prop_setInt::SetInt -> Bool
prop_setInt si = si == (unionEmpty' si )
test_setInt = verboseCheck prop_setInt 
````

When we run the test we can now see both the default arbitary generator and our specialised number generators are used. Some example results are as follows:

Passed:
SetInt {unSetInt = {7278365512726356731}}

Passed:
SetInt {unSetInt = {}}

Passed:
SetInt {unSetInt = {3}}

Passed:
SetInt {unSetInt = {-5406404320857615657,-4813399485135821273,-3005650030419867206,-1945620904159445367}}

Passed:
SetInt {unSetInt = {-74,-68,-66,-65,-64,-61,-58,-57,-56,-54,-53,-52,-48,-47,-46,-43,-42,-38,-37,-36,-32,-31,-30,-27,-22,-21,-15,-8,-3,0,2,7,19,20,22,23,28,31,35,37,38,40,44,46,47,49,50,51,53,55,60,61,63,67,68,71,72}}

Passed:
SetInt {unSetInt = {-75,-73,-72,-61,-59,-58,-53,-44,-43,-37,-30,-29,-4,-1,0,16,21,22,26,27,34,40,43,47,49,52,53,66,70}}

Passed:
SetInt {unSetInt = {-9115484458854837327,-9058000626678163118,-9002764207819162885,-8709772440515043519,-8588636706142451613,-8205618540893771088,-7642452169185312279,-7401776190629072481,-7232504125304008225,-7222035941057292048,-6708906479342843916,-6677938614993484412,-6599657050635151398,-6546964767920451365,-6342171489608621699,-6271421930491695255,-5816506866724209344,-5765708055546721864,-5390534107534648525,-5284385766654639563,-4933872724686635354,-4523756459895747623,-4455119157886008078,-4228272311384983487,-4218612067160069161,-4125567251396479432,-4109307185074127009,-3416724650656535048,-2886610050712219741,-2777568020295736089,-2724775635665003811,-2617633771264609256,-2547571807441720237,-2528892436736638570,-2490646013499916586,-2296650809023706601,-2291329811621891745,-2161771198007594445,-1703212632719173523,-1695928725508291616,-1558310219800547214,-1519839109685021301,-901358807404020276,-866217581564633777,-687748073892695017,-589360609784169870,-547888377359030923,-472801977892751728,-432077544547375490,-197488290140365809,-46694280980291962,262934995804261529,285200126993558410,456204596191414515,663980384341372405,771321120630826835,1236267298328021559,1542076347501052737,1633587947912910946,1803936971296594430,1912309852554925588,2703525003945503602,3255285745447437212,3272062815590075363,3323074104983502121,3413510321622823130,3565165656091754567,3988043892109143334,4012828864930773035,4027826581655573373,4192288031466682185,4525528191322255989,4565184131359250112,4623028101639127274,4652677627962708004,4723146422868853180,4859259302384513244,5041896149542491270,5233549026683246677,5541784264297423054,5544632449890632402,5959121660217938005,5972648897822945624,6051033721798517733,6184287520102174458,6217156587380545877,6307667820456007757,6311956441793327258,6351569864289734803,7148576587045811464,7360656970869297913,7525973775298692858,7655622971516809576,8059010155102177061,8105163830365863364,8490749060529242457,8582847246605022117,8846726730964806584,9099070347173901112}}

+++ OK, passed 100 tests.

### Adding shrink
QuickCheck has a number of tools to make traking down errors easier one of these is `shrink`.
To demonstrate the usefullness of this I will make a naive test, that in this case checks that there is never a 9 in a set that is produced:

````haskell 
prop_noNinesInSet::SetInt -> Bool
prop_noNinesInSet si = not $ inSet 9 (unSetInt si)
test_noNinesInSet = verboseCheck prop_noNinesInSet
````

I run my test and get the following:

*** Failed! Falsifiable (after 22 tests):
SetInt {unSetInt = {-21,-19,-18,-14,-11,-7,-4,2,8,9,10,15,18}}

It is nice to know that the test fails, however now I have to work through 13 cases to find out which one is causing the crash.

What `shrink` does is reduce your search space of failures by shinking the result to the smallest possible failure.

In order to shrink our SetInt I will unwrap the `Set Int`, pull out the list, using a helper method `set2list`, perform the default `shrink` on the list, which will create a list of possible shrink outcomes, I will then convert all these outcomes back into `SetInt`'s by mapping `(SetInt . list2set)` over the results.

````haskell
instance Arbitrary SetInt where 
    ...
    shrink si = map (SetInt . list2set) $ shrink $ set2list $ unSetInt si

set2list :: Set Int -> [Int]
set2list (Set ns) = ns
````

#### Testing With shrink

Running the test now will show the following results after a `SetInt` containing 9 has been found shrinks the results to show the smalled possible failing test is SetInt {unSetInt = {9}}:

Failed:
SetInt {unSetInt = {-13,-8,-2,-1,0,3,4,5,8,9}}

*** Failed! Passed:
SetInt {unSetInt = {}}

Failed:
SetInt {unSetInt = {3,4,5,8,9}}

Passed:
SetInt {unSetInt = {}}

Failed:
SetInt {unSetInt = {5,8,9}}

Passed:
SetInt {unSetInt = {}}

Failed:
SetInt {unSetInt = {8,9}}

Passed:
SetInt {unSetInt = {}}

Failed:
SetInt {unSetInt = {9}}

Passed:
SetInt {unSetInt = {}}

Passed:
SetInt {unSetInt = {0}}

Passed:
SetInt {unSetInt = {5}}

Passed:
SetInt {unSetInt = {7}}

Passed:
SetInt {unSetInt = {8}}

Falsifiable (after 14 tests and 4 shrinks):
SetInt {unSetInt = {9}}