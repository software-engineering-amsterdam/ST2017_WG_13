# Exercise 4

### Recognizing Permutations###
 
A permutation of a finite list is another finite list with the same elements, but possibly in a different order. For example, [3,2,1] is a permutation of [1,2,3], but [2,2,0] is not. 

1. Write a function ```	isPermutation :: Eq a => [a] -> [a] -> Bool```
that returns True if its arguments are permutations of each other.
2. Next, define some testable properties for this function, and use a number of well-chosen lists to test isPermutation. You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?
3. Provide an ordered list of properties by strength using the weakear and stronger definitions.
4. Can you automate the test process? Use the techniques presented in this week's lecture. Also use QuickCheck.

*Deliverables: Haskell program, concise test report, indication of time spent.*

#### 1. Writing the function ####

Precondtion(s)

	1) The elements of the inputlist are instances of Eq
	2) The inputlist does not contain duplicates

Postcondtion(s)

	1) The value returned reflects whether the given lists are permutations
	of each other.
	
First we make a function (operator) that tests whether all elements of given list are elements of another. Just for the fun of it, we will define an operator:

```haskell
(-<-) :: Eq a => [a] -> [a] -> Bool
left -<- right = all (True ==) [ x `elem` right  | x <- left  ]
```
With this operator in place we can define isPermutation as:

```haskell
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation left right = left -<- right && right -<- left
```
#### 2. Defining some properties for isPermutation ####

Any list is a permutation of itself:

```haskell
propSelfPermutation :: Eq a => [a] -> Bool
propSelfPermutation a =  isPermutation a a

```

The reverse of a list is a permutation of itself:

```haskell
propReverseInv :: Eq a => [a] -> Bool
propReverseInv a = isPermutation ( reverse a ) a
```

An easy way to test thes properties, is to use lists generated by:

```haskell
list1 = [-100,100]
list2 = ['a'..'z']
list3 = []
list4 = [()]
```
Neither of those will contain duplicates.

Testing the properties:

```
print $ propSelfPermutation list1
True
print $ propSelfPermutation list2
True
print $ propSelfPermutation list3
True
print $ propSelfPermutation list4
True
print $ propReverseInv list1
True
print $ propReverseInv list2
True
print $ propReverseInv list3
True
print $ propReverseInv list4
True
``` 

#### 3. Ordered list of properties ####

I was not sure how to determine this. My first guess is that they are of the same strength. 

#### 4. Automation ####

Running:

```haskell
verboseCheck propSelfPermutation

verboseCheck propReverseInv
```