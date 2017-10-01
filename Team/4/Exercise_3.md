# Exercise 4 #
Time Spent: 2 hours

## Specifications of problem ##
> Implement operations for set intersection, set union and set difference, for the datatype Set defined in SetOrd.hs. 
> Next, use automated testing to check that your implementation is correct. First use your own generator, next use QuickCheck.

There did not seem to be any ambiguity in the question

## Program
### Implementation

We had differing opinions on how to handle this. Should we reinvent the wheel? the consensus was: No. (however, if you wish to see the wheel being reinvented check out Excercise_3_orig.hs in Paul's folder).

As there is already an implentation in Data.List for intersection, union and difference, we decided to extract lists from two Sets, apply the list functions to those lists and then create a new Set with the resulting list.

```` haskell
intersect_SetInt :: (Ord a) => Set a -> Set a -> Set a
intersect_SetInt (Set a) (Set b) = list2set (intersect a b)

union_SetInt :: (Ord a) => Set a -> Set a -> Set a
union_SetInt (Set a) (Set b) = list2set (union a b)

difference_SetInt:: (Ord a) => Set a -> Set a -> Set a
difference_SetInt (Set a) (Set b) = list2set ( a \\ b)
````
### Testing
#### From Scratch tester
To test from scratch we first adjusted the tester from Exercise 2. We adjusted it to take in just one property that took in two `Set Int` arguments.  As we decided to do the testing within the properties we no longer needed to pass in the function to be tested.

````haskell 
setIntTester2 :: Int -> Int -> (Set Int -> Set Int -> Bool) -> IO ()
setIntTester2 current total property = 
    if current == total then 
        print (show total ++ " tests passed")
    else do
        si1 <- siGen 20        
        si2 <- siGen 20
        if property si1 si2 then
            do 
                print ("pass on: " ++ show si1++ " and  " ++ show si2)
                setIntTester2 (current+1) total property
        else 
            error ("failed test on: " ++ show si1++ " and  " ++ show si2)
````

#### Testing Intersect
To Test the implementation of `intersect_SetInt` we wanted to check that every item in the resulting set was in both of the input sets:

```` haskell
prop_InBothSets:: Set Int -> Set Int -> Bool
prop_InBothSets s1 s2 = all (==True) [(inSet x s1) && (inSet x s2) | x <- set2list s3  ]
    where 
        s3 = intersect_SetInt s1 s2
````

next we want to make sure that items that all items that are in each set are either in the intersection or not in the other set.  This pair of properties are as follows:

```` haskell
prop_IntersectOrNotSet2:: Set Int -> Set Int -> Bool
prop_IntersectOrNotSet2 s1 s2 = all (==True) [(inSet x s3) || (not $ inSet x s2) | x <- set2list s1  ]
    where 
        s3 = intersect_SetInt s1 s2

prop_IntersectOrNotSet1:: Set Int -> Set Int -> Bool
prop_IntersectOrNotSet1 s1 s2 = all (==True) [(inSet x s3) || (not $ inSet x s1) | x <- set2list s2  ]
    where 
        s3 = intersect_SetInt s1 s2
````

We test the properties of intersect with both our from scratch tester and with quickCheck using the following code:

```` haskell
test_InBothSets          = setIntTester2 1 100 prop_InBothSets
test_InBothSets'         = verboseCheck prop_InBothSets
test_IntersectOrNotSet1  = setIntTester2 1 100 prop_IntersectOrNotSet1
test_IntersectOrNotSet1' = verboseCheck prop_IntersectOrNotSet1
test_IntersectOrNotSet2  = setIntTester2 1 100 prop_IntersectOrNotSet2
test_IntersectOrNotSet2' = verboseCheck prop_IntersectOrNotSet2
````
#### Testing Union
The first property of union that we want to test is that all the items in the result exist in one of the input sets:

```` haskell
prop_inAtLeastOneSet:: Set Int -> Set Int -> Bool
prop_inAtLeastOneSet s1 s2 = all (==True) [(inSet x s1) || (inSet x s2) | x <- set2list s3  ]
    where 
        s3 = union_SetInt s1 s2
````

Next we want to make sure that there is nothing in either of the input sets that is not in the result of the union:

```` haskell
prop_AllSet1InUnion:: Set Int -> Set Int -> Bool
prop_AllSet1InUnion s1 s2 = all (==True) [(inSet x s3) | x <- set2list s1  ]
    where 
        s3 = union_SetInt s1 s2

prop_AllSet2InUnion:: Set Int -> Set Int -> Bool
prop_AllSet2InUnion s1 s2 = all (==True) [(inSet x s3) | x <- set2list s2  ]
    where 
        s3 = union_SetInt s1 s2
````

finally, as there is already a union implementation within SetOrd.hs for `Set a`, we figured that we could test equivalence with that oracle:

```` haskell
prop_matchedInternalImpl::Set Int -> Set Int -> Bool
prop_matchedInternalImpl s1 s2 = (unionSet s1 s2) == (union_SetInt s1 s2)
````

to run these tests we have the following code:

```` haskell
test_inAtLeastOneSet = setIntTester2 1 100 prop_inAtLeastOneSet
test_inAtLeastOneSet' = verboseCheck prop_inAtLeastOneSet
test_AllSet1InUnion = setIntTester2 1 100  prop_AllSet1InUnion
test_AllSet1InUnion' = verboseCheck  prop_AllSet1InUnion
test_AllSet2InUnion = setIntTester2 1 100  prop_AllSet2InUnion
test_AllSet2InUnion' = verboseCheck  prop_AllSet2InUnion
test_matchImpl = setIntTester2 1 100 prop_matchedInternalImpl
test_matchImpl' = verboseCheck prop_matchedInternalImpl
````
#### Testing difference

To test difference we checked with an exclusive or on the result coming from either of the input (and then obviously reversed this with a not).

```` haskell
prop_NotInBothSets:: Set Int -> Set Int -> Bool
prop_NotInBothSets s1 s2 = all (==True) [not $ insetXor x s1 s2 | x <- set2list s3  ]
    where 
        s3 = difference_SetInt s1 s2
        insetXor x s1 s2 = ((inSet x s1) || (inSet x s2)) && (not (inSet x s1) && (inSet x s2))
````

We did not thing any extra tests would be needed than this.
To test this the following code can be used

```` haskell
test_NotInBothSets = setIntTester2 1 100 prop_NotInBothSets
test_NotInBothSets' = verboseCheck prop_NotInBothSets
````

#### Testing report
To run the above described tests there would be 1600 tests to report on. All the tests pass, as a convenience we provide code to run all of the tests `testAll`, for the from scratch tests and `testAllQc` for the quickCheck test:

```` haskell
testAll = do
  test_InBothSets
  test_IntersectOrNotSet1
  test_IntersectOrNotSet2
  test_inAtLeastOneSet 
  test_AllSet1InUnion
  test_AllSet2InUnion
  test_matchImpl
  test_NotInBothSets
  
testAllQc = do
  test_InBothSets'
  test_IntersectOrNotSet1'
  test_IntersectOrNotSet2'
  test_inAtLeastOneSet'
  test_AllSet1InUnion'
  test_AllSet2InUnion'
  test_matchImpl'
  test_NotInBothSets'
````

some example from scratch test results:
"pass on: {-5920004113038085841} and  {-4378522279343935046}"

"pass on: {-7932714933657871381,-7386239504277324241,-4678273147973845365,-4223322676912361533,-2336773677058714059,1997454160802959755,4741208646086363149,6451528658656443744} and  {-5069502624334718678,-4582305454376557439,-3938136318265536432,64315309390974187,379869109662484711,693870998941818324,939418812690938626,1961402229476312117,2252990631889950067,7362168814940057448}"

"pass on: {-8797635285713211936,-8027403341147365024,-5621218398221082868,-5359358589952400655,-5286325238712642755,-4140213949615672013,-2832023455098916545,-2830268785623943871,-1526888152435561036,-1172708794269477316,138509411881392020,211335904163345515,683310348461052815,2502141941579697514,3913188775702420742,6423746896614282149} and  {-7932714933657871381,-7202957711343729783,-4678273147973845365,-4655719011719074748,-4223322676912361533,-4080868416682728877,-3217147163751280272,-2336773677058714059,16966585903563223,765982431598794893,834367791308054313,1508831207319998102,1650797805359308091,1997454160802959755,2794218341922567401,4003436325474477278,4741208646086363149,6451528658656443744,7828639281977688084}"

"100 tests passed"

some examples from the quickCheck tests:

Passed:
{7}
{-4,6}

Passed:
{-84,-76,-67,-64,-48,-47,-45,-43,-34,-30,-19,-7,-4,-3,-1,4,20,33,42,53,70,79,80}
{18,48}

Passed:
{-96,-95,-92,-90,-85,-82,-80,-79,-77,-76,-75,-74,-70,-67,-66,-65,-59,-56,-53,-52,-51,-46,-43,-40,-38,-37,-31,-28,-26,-25,-21,-19,-17,-13,-10,-9,-8,-6,-4,-2,0,12,13,15,22,23,24,26,28,31,33,46,50,51,52,54,57,58,62,63,71,74,78,80,86,87,89,90}
{-97,-94,-90,-89,-88,-87,-86,-80,-77,-76,-73,-70,-66,-60,-59,-58,-57,-55,-37,-36,-35,-33,-28,-26,-25,-22,-21,-20,-15,-8,-7,-4,-3,-1,0,1,12,15,17,19,23,32,35,37,42,48,51,56,65,67,74,77,78,83,85,90}

Passed:
{-91,-72,-31,-27,-18,-5,21,28,39,52,62}
{-98,-96,-93,-92,-90,-88,-85,-81,-79,-76,-69,-68,-66,-62,-55,-50,-40,-38,-35,-33,-29,-24,-22,-21,-18,-16,-14,-13,-7,-4,0,6,10,14,18,21,22,31,37,38,49,51,54,63,70,71,72,77,78,79,80,88}

Passed:
{-96,-77,-73,-72,-69,-53,-47,-46,-45,-37,-36,-24,-21,-19,-12,-10,-8,-5,-2,8,12,22,32,35,39,45,47,50,55,56,62,64,69,74,76,77,78,87,99}
{}

+++ OK, passed 100 tests.