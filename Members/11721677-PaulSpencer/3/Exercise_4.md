# Exercise 4 #
Total Time spent 6 hours - had to rewrite to make understandable

## Specifications of problem ##

To create automated tests using QuickCheck (or other method) to test that the CNF convertor is generating a logic formula in Conjunctive Normal Form that is logically equivalent to the input formula that it is converting.

To Test for logical equivalence we will use the equiv function that was generated in Exercise 1 to test that an arbitrarily generated formula is equivalent to the output of the CNF convertor created in Exercise 3.

## Specifications of generator ##
We chose to use QuickCheck to do our automated testing.  A prerequesite of using QuickCheck is that the Type you are testing has to implement the Arbitrary class

For our tests we would like to generate an arbitary length formula. 
The functional specifications for this are that the formula should:
* Contain at least one proposition
* Contains zero or more of 
    * negation
    * implication
    * equivalance
    * disjunction
    * proposition
* There can be many different proposition names, but there should be a bias for names repeating
* There can be lots of items in a conjunction or disjunction, but there should be a bias towards shorter deeper trees, which implies a bias towards shorter lists of disjunctions

The non-functional specifications for this are that the formula should:
* Terminate
* Be easy to read the source code for those new to QuickCheck 

## Program ##
The idea for this program came from the paper: Quick Check A Lightweight Tool for Random Testing of Haskell Programs, by Koen Claessen, John Hughes, specifically the section 3.2 Generators for User-Defined Types
https://www.eecs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf

I was having particular problem with reusing randomly generated variables, thus the generation of lists of formulas is inspired by the answer of Carl (no further details available) from this question : https://stackoverflow.com/questions/25300551/multiple-arbitrary-calls-returning-same-value

Particular attention was paid to naming of both types and functions with a focus on understandability for a newcomer at the cost of the normal cryptic style. Whilst this might cost us points, at least we will be able to understand what we wrote, when we look back at it.

In order for quickcheck to generate tests for a type, that type has to implement the class Arbitrary, which has the arbritary function which returns an arbritary value of the type. It is noted in the Claessen, Hughes paper that one should be carefull with recursive types to limit the chance of an infinite type.  The mechanism we use for this is QuickChecks sized function. we use the input of sized as a terminating reducer for our User-Defined Type generator:  arbitraryFormulaGenerator.

#### formula generator
first we create a generator that will terminate formula branches with proposition leaves.   

The max value is reduced on each branch of the formula, when a branch path of the generator reaches a max of 0 then it terminates with a proposition.

````haskell
instance Arbitrary Form where
    arbitrary = sized arbitraryFormulaGenerator

arbitraryFormulaGenerator ::RemainingOperatorCount -> LogicFormulaGenerator
arbitraryFormulaGenerator 0 = propositionGenerator
arbitraryFormulaGenerator max = oneof [
    (negationGenerator max),
    (implicationGenerator max),
    (equivilanceGenerator max),
    (disjunctionGenerator max),
    (conjunctionGenerator max),
    (propositionGenerator)]
````

for readability we Retyped `Int` to be `RemainingOperatorCount` and `Gen Form` to be `LogicFormulaGenerator`

````haskell
type LogicFormulaGenerator = Gen Form
type RemainingOperatorCount = Int
````

Until the a branch path has terminated one of 7 generators of Logic Formulas will be arbitarily picked by QuickCheck

#### proposition generator
propositions have identifiers that are of type Name which is a reTyping of Int.  Whilst we would like to check a wide variety of Ints, it is important that they are positive, as negative Names will interfere with the parsing, a parser will not be able to tell if the negative Name is "-1" or if it is a negation of the Name 1.  It was also important that we had a bias towards multiple occurences of similar Names in the formula. 

To achieve this Bias we used QuickChecks frequency function in the following code we have biased that 50/100 of times quickcheck will choose a name for a proposition of 1,2 or 3, 30/100 times between 4 and 7, 15/100 times between 8 and 20 and the remaining 5/100 times some larger number. 

````haskell
propositionGenerator:: LogicFormulaGenerator
propositionGenerator = frequency [
    (50, propositionGeneratorWithIdBetween 1 3),
    (30, propositionGeneratorWithIdBetween 4 7),
    (15, propositionGeneratorWithIdBetween 8 20),
    (5, propositionGeneratorWithIdBetween 21 10000)]

propositionGeneratorWithIdBetween::Int -> Int -> LogicFormulaGenerator
propositionGeneratorWithIdBetween min max = liftM Prop (choose (min, max))
````

To genererate the Proposition we use the liftM function to lift the `Prop` constructor into the int generator monad (`Gen Int`) that the function choose creates to return a logic formula generator (`Gen Form` / `LogicFormulaGenerator`). As the proposition generator does not recurse `arbitraryFormulaGenerator`, this proposition is now the terminating leaf of this branch of the formula.

**Discussion**
Because there is a 1 in 7 chance that this will be picked first, there is a 1 in 7 chance that our tree will only be 1 proposition long, which is valid.  For clarity we chose to use `oneof` to select this generator, rather than weighting it lower by using frequency. We discussed whether or not to allow the selection of the proposition generator before reaching the natural end of a branch and decided to allow it to allow for much more naturally unbalanced formulas


#### negation generator
The negation generator is simple, it applys the negation constructor to a formula created by recursively calling the `arbitraryFormulaGenerator`. Because the `arbitraryFormulaGenerator` is of a monadic type it uses, as will the rest of the generators, `liftM` to put the constructor with in the `LogicFormulaGenerator`. 
As negation only has one branch we reduce the maximum remaining formulas on that branch by 1.

````haskell
negationGenerator :: RemainingOperatorCount -> LogicFormulaGenerator
negationGenerator max = liftM Neg (arbitraryFormulaGenerator (max - 1))
````

#### implication and equivalance generators

The implcation and equivalence generators are binary operators and therefore need require a formula to be generated for both sides. If we were to just reduce the remaining operator count by 1 then because the recursion of the `arbitraryFormulaGenerator` has now split we would have a growth in possible formulas of approximatly n!/2, to prevent this growth we reduce the `RemainingOperatorCount` that we pass to each branch by half using the `updatedMax` function

````haskell
implicationGenerator :: RemainingOperatorCount -> LogicFormulaGenerator
implicationGenerator max = binaryLogicFormulaGenerator Impl max

equivilanceGenerator :: RemainingOperatorCount -> LogicFormulaGenerator
equivilanceGenerator max = binaryLogicFormulaGenerator Equiv max

binaryLogicFormulaGenerator:: BinaryLogicOperation -> RemainingOperatorCount -> LogicFormulaGenerator 
binaryLogicFormulaGenerator binaryLogicOperation max = 
    liftM2 binaryLogicOperation 
        (arbitraryFormulaGenerator $ updatedMax max 2) 
        (arbitraryFormulaGenerator $ updatedMax max 2)

updatedMax::RemainingOperatorCount -> BranchCount -> RemainingOperatorCount
updatedMax maxRemainingFormulas branchCount = maxRemainingFormulas `div` branchCount
````

in order to not repeat the same code for `Impl` and `Equiv`, the function `binaryLogicFormulaGenerator` was created that took in the constructors `Impl` and `Equiv`, which we had reTyped to `BinaryLogicOperation` for readability:

````haskell
type BinaryLogicOperation = (Form -> Form -> Form)
````
#### conjunction and disjunction generators
conjunctions and disjunctions are constructed with a list of formulas.
We decided that although we wanted to test long lists of formulas for conjunctions and disjunctions we would prefer to check a shorter list of conjunctions. Thus, in `listOfLogicOperatorsGenerator` we create 80/100 lists between 2 and 3 formulas, 19/100 have between 4 and 10, and 1/100 have upto 50 formulas.
Having multiple recursively created formulas has the same issue of size explosion, except much larger (instead of n^2, we can now have a - very small - possiblity of n^50 growth of potential formumula generation), thus when we are reduce the size of the number of formulas on the subsequent branch by a divisor of the number of branches created.

````haskell
disjunctionGenerator :: RemainingOperatorCount -> LogicFormulaGenerator
disjunctionGenerator max = listOfLogicOperatorsGenerator Dsj max

conjunctionGenerator :: RemainingOperatorCount -> LogicFormulaGenerator
conjunctionGenerator max = listOfLogicOperatorsGenerator Cnj max


listOfLogicOperatorsGenerator::MultipleLogicOperation -> RemainingOperatorCount -> LogicFormulaGenerator
listOfLogicOperatorsGenerator multipleLogicOperation max = frequency [
    (80, liftM multipleLogicOperation (listOfLogicOperatorsGeneratorBetween max 2 3)),
    (19, liftM multipleLogicOperation (listOfLogicOperatorsGeneratorBetween max 4 10)),
    (1, liftM multipleLogicOperation (listOfLogicOperatorsGeneratorBetween max 11 50))]

listOfLogicOperatorsGeneratorBetween::RemainingOperatorCount -> Int -> Int -> MultipleLogicFormulaGenerator
listOfLogicOperatorsGeneratorBetween maxRemaining minLength maxLength = oneof $ 
    [listOfLogicOperators maxRemaining actualLength | actualLength <- [minLength..maxLength] ]

listOfLogicOperators::RemainingOperatorCount -> BranchCount -> MultipleLogicFormulaGenerator
listOfLogicOperators max len = replicateM len (arbitraryFormulaGenerator (updatedMax max len))
````

`listOfLogicOperatorsGeneratorBetween` was inspired by https://stackoverflow.com/questions/25300551/multiple-arbitrary-calls-returning-same-value to deal with an issue we were having with reusing the randomly generated number for both the length of the function list and the redution factor of the remaining possible function count.

## Testing generator ##
first we tested this by eye, using QuickChecks `generate` function.  
To have an approximation that this will teminate we ran it with an limit of 100,000,000 functions. This took several minutes to complete, but it did terminate.




### sequence of test properties ###

### test report ###

## Testing CNF convertor ##

### sequence of test properties ###

### test report ###