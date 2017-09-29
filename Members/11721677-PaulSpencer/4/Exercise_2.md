# Exercise 4 #
Total time spent ~5 hours as follow:
* 20 minutes to implement the random test generator
* 30 minutes to adapt the random code runner from lesson 2's code, and write a test
* 2 minutes to use QuickCheck to generate a random list to feed into Set
* 120 minutes banging my head against the wall trying to work out that you cant use a random monad within a Gen mondad
* 30 minutes building a Gen (Set a) plus tests
* 30 minutes working out that I needed to wrap Gen Int in a newtype GenInt and making tests
* 45 minutes working out how shrink works and implementing

## Specifications of problem ##
> Implement a random data generator for the datatype `Set Int`, where Set is as defined in SetOrd.hs. 
> First do this from scratch, next give a version that uses QuickCheck to random test this datatype.

For the first part, there needs to be two random parts, namely the list length and the list content of the integers that will be used to create the `Set Int` value.

The second part is open to interpretation, the question asks to use QuickCheck to test this data type, this could either mean use QuickCheck to generate the input data to test the type or use QuickCheck to generate arbitrary instances of `Set Int` to use in QuickCheck tests.

## Program ##
###Random Generator

To create the `Set Int` we chose to use the `list2set` function, rather than doing using the `Set [a]` constructor, as experiments showed that using the constructor did not create a value that behaved like an ordered set, whereas `list2set` did. In order to call `list2set`, a list of values of the class `Ord` has to be produced, as we are testing `Set Int` then the value we are generating will be of the type `[Int]`.

We could generate our own random numbers, however reinventing the wheel seems a bit excessive, so we will use the `System.Random` library to help us. As the use of the global random number generator is in the `IO` monad our `Set Int` generator will be of type `IO (Set Int)`.

To generate the random length of the list, from `System.Random` we use `getStdRandom` to access values from the current global random generator, and `randomR` to bound the range of values we get back from 0 (empty list) to a parameter value we pass in to limit how big the list will be, (naturally we do not want to be creating really long lists every time we test.)

To generate the random list of integers we first get the global random generator with `getStdGen`, to be the input for the function randoms, which will generate an infinite list of values of the class `Random a`.  As we want this to be a list of `Int`, we give the function the type `[Int]`.

Finally, to get a list of a random size, we use `take` and  the randomly generated length on the the randomly generated infinite list.  This list is then fed has  `list2set` applied to it .

````haskell
siGen::Int -> IO (Set Int)
siGen n = do
    l <- getStdRandom (randomR (0,n))
    g <- getStdGen
    return $ list2set $ take l (randoms g :: [Int])
````

####Testing Random Generator
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

then we made a property, `prop_areEqual` - to check for equality, and a function, `unionEmpty` - to union a set with an empty set, to be used in the tester, `test_areEqual`:

````haskell
prop_areEqual::Set Int -> Set Int -> Bool
prop_areEqual x y = x == y

unionEmpty::Set Int -> Set Int
unionEmpty si = unionSet si emptySet

test_areEqual::IO()
test_areEqual = setIntTester 1 100 unionEmpty prop_areEqual
````

Whilst this does not show that the values are random it does show that it at least produces valid values

###Test (List Int) with QuickCheck
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

###Generate (Set a) with QuickCheck
For QuickCheck to generate tests for a type, that type has to implement the class `Arbitrary`, which has the `arbitrary` function which returns an arbitrary value of the type. 

First off we created an `Arbitrary` instance of the type `Set a`. To do this we allowed quickcheck to pick the maximum size of the input list by using the `sized` function.  next we applied the `sized` function to a lambda that did the following
1. Took in a maximun list length `\n`
1. generated an list of arbitrary values using `arbitraryListGen` which
  1. picks an arbitrary number between 0 and the parameter
  1. the number is bound to `vector` that will create a list of the lenght of the bound number of arbitrary values
1. applies `list2set` to this list to create a value of type `Set a`
1. wraps the value in a `Gen (Set a)` monad using return

````haskell
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = sized (\n -> do
        lst <- arbitraryListGen n
        return $ list2set lst)

arbitraryListGen::(Ord a, Arbitrary a) => Int -> Gen [a]
arbitraryListGen n = choose (0,n) >>= vector
````

####Testing (Set a) with QuickCheck
Now that we have implemented `Arbitrary` to `Set a` we use QuickCheck to generate a Set Int, as follows:

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

###Generate (Set Int) with QuickCheck
Implementing `Arbirary` for `Set a` has two major disadvantages. Firstly, and most importantly to this exercise, it could be argued that this is not what was asked for. The Question asks us to use QuickCheck to test this datatype, and to an extent we are checking `Set a` and not `Set Int`. Secondly, and more importantly for us, by only testing `Set a` we cede control of the generation of data points to that that can be generated for data types that implement `Ord`.  This means that the above test results, which are created by the default implementation of `Arbitrary Int` when generated within `vector`.

It is not posible to create an `Arbitrary (Set Int)` implementation without using `{-# LANGUAGE FlexibleInstances #-}` GHC language extension.  As this would affect portablity of the code to other compilers, we chose to instead use `newtype` to wrap `Set Int`:

````haskell 
newtype SetInt = SetInt { unSetInt :: Set Int } deriving (Eq, Show)
````

now we could go ahead and specifically test `Set Int` by implementing `Arbitrary` on our new type `SetInt`.  This has the extra overhead of packing and unpacking the type, however it gives us the advantage of being able to add our own random integer generation `intListGen`.  We decided that the numbers created whilst usefull were not satisfactory, so we wanted to generate larger numbers.  To do this we use `choose (minBound, maxBound)` to pick any valid integer and then recursively build a list. In our instance of `Arbitrary` we use `oneof` to call either arbitraryListGen or arbitraryListGen. 

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

###Testing Generate (Set Int) with QuickCheck
wrapping `Set Int` makes testing a little more cumbersome, to do the same test that a union with an empty test is the same means we have to write a new function for `unionSet` and `unionEmpty` to handle the new type `SetInt`:

When we run we can now see both the arbitary and our specialised number generators are used:

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

some exampel results are as follows:

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

###Adding shrink
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

It is nice to know that the test fails, however now I have to work through 13 cases to find out which one is equal to 9 and thus causing the crash.

What `shrink` does is reduce your search space of failures by shinking the result to the smallest possible failure.

In order to shrink our SetInt I will unwrap the Set Int, pull out the list, using a helper method set2list, perform the default shrink on the list, which will create a list of possible shrink outcomes, I will then convert all these outcomes back into `SetInt`'s my mapping `(SetInt . list2set)` over the results.

````haskell
instance Arbitrary SetInt where 
    ...
    shrink si = map (SetInt . list2set) $ shrink $ set2list $ unSetInt si

set2list :: Set Int -> [Int]
set2list (Set ns) = ns
````

###Testing With shrink

running the test now will show the following results after a SetInt containing 9 has been found shrinks the results to show the smalled possible failing test is SetInt {unSetInt = {9}}:

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

