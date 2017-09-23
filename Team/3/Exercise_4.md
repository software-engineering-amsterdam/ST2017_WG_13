# Exercise 4 #
Total Time spent 8 hours - re-wrote multiple times

## Specifications of problem ##

To create automated tests using QuickCheck (or other method) to test that the CNF convertor is generating a logic formula in Conjunctive Normal Form that is logically equivalent to the input formula that it is converting.

To Test for logical equivalence we will use the equiv function that was generated in Exercise 1 to test that an arbitrarily generated formula is equivalent to the output of the CNF convertor created in Exercise 3.

## Specifications of generator ##
We chose to use QuickCheck to do our automated testing.  
A prerequesite of using QuickCheck on a Type is that the Type you are testing implements the Arbitrary class.

The generation of a Formula is similar to the generation of a tree, as it is recursive. Koen and Hughes warn of the danger of recursive Type generation in their paper : Quick Check A Lightweight Tool for Random Testing of Haskell Programs, by Koen Claessen, John Hughes, specifically the section 3.2 Generators for User-Defined Types
https://www.eecs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf.

We disagreed with the solution of Hughes and Claessen, specifically because they are generating a balanced tree. We decided to make our formula unbalanced to more realistically reflect real formulas.

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
* There can be lots of items in a conjunction or disjunction, but there should be a bias towards shorter deeper trees, which implies a bias towards shorter lists of disjunctions.
* the formula can be arbitrarily balanced.

The non-functional specifications for this are that the formula should:
* Terminate

## Program ##

In order for quickcheck to generate tests for a type, that type has to implement the class Arbitrary, which has the arbritary function which returns an arbritary value of the type. It is noted in the Claessen, Hughes paper that one should be carefull with recursive types to limit the chance of an infinite recursion.  The mechanism we use for this is QuickChecks' `sized` function. we use the input of sized as a terminating reducer for our User-Defined Type generator:  `formGen`.

````haskell
instance Arbitrary Form where
    arbitrary = sized formGen
````

#### formula generator
first we create a generator that will terminate formula branches with proposition leaves.   

When the remaining operators on a branch has reached zero then the formulas branch is terminated with a proposition.

if there is one operator then the negation operator is used as it is the only operator that uses one operator.

in other cases we arbitrarily choose a generator of the next operator.  To do this we generate an arity of the next operator.  We chose to do this, because the profile of the different opreators is different dependant on their arity, for example Neg has an arity of one and takes a single argument, Impl and Equiv take to arguments, whereas Cnj and Dsj can have flexible arities, and thus to simulate this take a list as an arguement.

````haskell
formGen ::Int -> Gen Form
formGen s 
    | s <= 0    = propGen
    | s == 1    = negGen s
    | otherwise =  do
        arity <- arityGen
        frm <- (case arity of
                One -> negGen s
                Two -> imEqGen s
                Many -> cjDjGen s)
        return frm
````

The arity generator takes into account that there are different number of operators in each of the arity goups.

````haskell
data Arity = One | Two | Many

arityGen::Gen Arity
arityGen = elements [One, Two, Two, Many, Many]
````

#### proposition generator
propositions have identifiers that are of type Name which is a reTyping of Int.  Whilst we would like to check a wide variety of Ints, it is important that they are positive, as negative Names will interfere with the parsing, a parser will not be able to tell if the negative Name is "-1" or if it is a negation of the Name 1.  It was also important that we had a bias towards multiple occurences of similar Names in the formula. 

To achieve this Bias we used QuickChecks frequency function in the following code we have biased that 60/100 of times quickcheck will choose a name for a proposition of 1,2 or 3, 30/100 times between 4 and 7, 9/100 times between 8 and 20 and the remaining 1/100 times some larger number. 

````haskell

propGen::Gen Form
propGen = do
    id <- propIdGen
    return (Prop id)

propIdGen::Gen Int
propIdGen = frequency [
    (60, choose(1, 3)),
    (30, choose(4, 7)),
    (9, choose(8, 20)),
    (1, choose(21, 10000000))]
````

#### distributing remaining operators
The following code is used by all the remaining generators, thus we will explain it here.

Suppose you have a recursive branching algorithm that you wish to end. To end it you give it a terminating value of 5. On each operand you reduce the value by 1. If your algorithm branches with only operand (e.g. Neg), then the total number of proposition would be 5. If your branch only had 2 operand operations at each branch the count would reduce by 1 but the number of brances doubles thus you would have 101 propositions ((1*5)+(2*4)+(3*8)+(2*16)+(1*32)).  This explosion is even bigger when considering that conjunctions and disjunctions can contain a very large number of operators.

The solution that Claessen and Hughes propose is to use `s div 2` on the number of operations, an a binary branch (and presumably `s div n` on a branch with an arity of n).  We find this unsatisfactory for 2 reasons. Firstly there is information loss.  At every uneven division, one operand is thrown away, thus if you use this method expecting to have 31 operations you would have 31 operations, however if you expect 32 operations you get 63 operations. This lack of control over input and output is not desired

More importantly this does not reflect the real world.  Equasions can be bothed balanced and unbalanced, thus at we would like to arbtirarly distribute the remaining operators (i.e. current remaining -1) accross the branches.  With Neg this does not matter as we can only distribute across  one branch. lets say we wanted to distribute 4 remaining operators across 2 branches, we could distribute them (l, r)=[(0,4),(1,3),(2,2),(1,3),(0,4)], where (2,2) would be balanced as Claessen and Hughes suggest, this would leave 4 options unexplored. a zero would indicate that the next item in the formula is a proposition.  
Distributing across a pair is relatively simple, pick a random number between 0 and the remaining operators, for one side and remove that value from the total for the other.  however distributing across a list recursivly using the above method skews the number of operands across the list, thus it is necessary to divide the random number generated by a random number between 1 and the remaining size of the list, see `boundedDistributionGen`, to allow values to distribute evenly across the list. 
naturally before you do all this you should reduce the reamining operators by 1.

````haskell
formDistributionGen:: Int -> Int -> Gen [Form]
formDistributionGen s len = fdg (s-1) len
    where
        fdg s' 1 = do
            f <- formGen s'
            return [f]  
        fdg s' len' = do
            rn <- boundedDistributionGen s' len'
            fs <- fdg (s'- rn) (len'-1)
            f <- formGen rn
            return (f : fs)

boundedDistributionGen::Int -> Int -> Gen Int
boundedDistributionGen num len = do
    rn <- choose (0, num)
    rd <- choose (1, len) 
    return (rn `div` rd)
````

#### negation generator
We generate a list that randomly distributes the remaining operatots over a list of size 1, and take the first item.

````haskell
negGen:: Int -> Gen Form
negGen s = do
    fs <- formDistributionGen s 1
    return (Neg (head fs))
````

(ok, this might be re-use gone mad, however most of my bugs came from not reducing the remaining operators in too many places, so I just leave that reduction in one place the `formDistributionGen` function)

#### implication and equivalance generators
We generate a list that randomly distributes the remaining operatots over a list of size 2.  We take the first item as the first argument and the last item as the second argument.  we randomly pick either an Impl or an Equiv constuctor.

````haskell
imEqGen:: Int -> Gen Form
imEqGen s = do
    fs <- formDistributionGen s 2
    ctor <- elements [Impl, Equiv]
    return (ctor (head fs) (last fs))
````

#### conjunction and disjunction generators
We generate a list that randomly distributes the remaining operatots over a list of an arbirarily chosen size and supply it as the argument.  We randomly pick either a Dsj or a Cnj constuctor.

````haskell
cjDjGen:: Int -> Gen Form
cjDjGen s = do
        len <- lenGen 
        fs <- formDistributionGen s len
        ctor <- elements [Dsj, Cnj]
        return (ctor fs)
````

We wanted to generate more deep functions than shallow, so our generator for the size of the number of arguments for our conjunctions or disjunctions heavily favours shorter lists. 90/100 will have a length of 2-3, 9/100 will have a length of 4-10 and 1/100 will have a length between 11 and 50.

````haskell
lenGen::Gen Int
lenGen = frequency [
    (90, choose(2, 3)),
    (9, choose(4, 10)),
    (1, choose(11, 50))]
````

## Testing generator ##
first we tested the `formGen` function by eye, using QuickChecks `generate` function.  
We ran it with an limit of 1,000,000 functions. This took 2:58 to complete running in ghci on a 2 core surface pro.  This gave us confidence that it would terminate.

To give us more confidence of termination we wrote a test that would confirm that the number of operators requested by `formGen` was the same as those produced.  We considered making `Form` a functor, so that we could use fmap for counting the operators, however we chose the easier path of counting the operators of the displayed formula.  The substring counter inpired by http://www.programming-idioms.org/idiom/82/count-substring-occurrences/999/haskell)

````haskell
opCount ops s = sum [ 1 | r <- tails s, ss <-ops , isPrefixOf ss r ]

genProp_validOpCount = do
    s <- choose (0,10000)
    form <- formGen s
    return ((opCount ["-","==>","<=>","*(","+("] $ show form) == s) 
test_ValidOpCount = verboseCheck $ forAll genProp_validOpCount (==True)
````

Now that we were fully confidant that we had avoided the naive traps in recursive testing of non-termination and size explosion, we moved on to validity. This was relatively simple, we use QuickCheck to arbitrily create Forms and, relying on show and parse to be reflexive we compare the output of parse of show of the formula with the original formula.

````haskell
prop_validForm form = (head $ parse $ show form) == form
test_validForm = verboseCheck prop_validForm
````
further tests that should be performed, but have not yet would be to confirm the arbitrary nature of distributions not only of depth and operators but also of formula balance.

## Testing CNF convertor ##

This is again relatively simple.  We use the eqiv formula from Exercise_1 and compare an arbitrarily created formula with the formula that has had the cnfGenerator from Exercise_3 applied to it.

````haskell
prop_validCnfConversion form = equiv form (cnfGenerator form)
test_validCnfConversion = verboseCheck prop_validCnfConversion
````

Whilst the quickCheck run was successfull it was slow, taking an average of 1:39, to complete 100 tests over 5 runs.

Below are some example test results of test_validCnfConversion:

Passed:
6

Passed:
-1

Passed:
+(1 1 -2)

Passed:
*((3<=>-(-1==>19)) *(1 1 -9))

Passed:
-+(*(1 6 (-7<=>-1)) +(-12 *(-1 2) +(-1 (6<=>(-5==>-14)))) (-(--(4<=>-3)==>--5)==>(-11==>*(-5 -6 (--2<=>6)))))

Passed:
-(*(4 (1==>-2) (3==>-1))<=>-*(3 *(-(4<=>*(5 (5==>-1))) *(2 (-4==>6))) (6==>-3) --2 17 *(-4 -3 -*(2 (15<=>-3))) 4 (5==>-3) 1 +(-6(-3<=>+(-3 (12==>-2) 1 2 3 2 3)))))


Passed:
(((5<=>((*(11 7493636 -7)<=>(4==>-3))==>+((6==>(3==>-5)) *(5 (-5==>(1==>-1))))))==>+(-3 -(*(-3 (*(-5 3 (1<=>--3))<=>--+(4 *((11<=>-+(4 1 2 2 (5==>-7) 7 5 3 3)) 1))))<=>*(20 -2 4))))==>-+(((((*(1 -2)<=>-(14==>-1))<=>-+(2 1 -3 8035849 -2 --4 1 -13))<=>3)==>*(+(-10 5 1 3 -2) -((3<=>(+(+(3 -7) (3==>-5))<=>*(1 3 -6 -2 4 -1)))<=>1) 7)) +(-((-5<=>-2)==>2) +(2 (4==>-5)) -+(4 *(3 5 -4) (6<=>+(
1 (-11<=>-2)))))))

Passed:
*(-6 ((-*(+(2 4 --3) -4)<=>*((-1<=>((-1==>2)<=>1)) 2 +((5==>-17) *(-4 +(1 (10<=>(-3<=>-3)) +((4<=>(-3<=>(*(3 3 -5)<=>12))) -4 3 3 8876266 3 5 15 -3))))))==>(1<=>+(*(-3 -2) -2 7))) (-(5<=>-18)<=>(-((-3==>*(2 *(-12 6 +(-4 2)) 1))==>-+(+((+(*(9 -1) 4 3 2 2)==>*(1 1 4 -2 3 2 -3)) -+(-2 4 (-12==>-+((-3<=>7) 6582051)))) *(2 1 6 18 1 2 -1 7 +((697607==>-4) +(5 (*(-2 1)==>4))) 9 4 1 19 2 4 9 3 15 4)))<=>((-1<=>(5==>-2))==>(+(+(-7 -11) --4 7)<=>2)))))

Passed:
+(*((((1<=>-2)==>-2)<=>((*(1 7 5 (2==>-9796060==>1)))==>2)<=>*(-1 --9 -(7==>-3)))) (3==>-1)) -+(*(-3 2 -13 3 1 -1 -1) +((-*(3 --2 1)<=>*(-(-1<=>-4) -2)) *(+(6 3 2 7 2 19 3 2 1 5 2 2 3 3 6 -4 5 2 4 3 +(2 10 -2) 3 3 3 5 1 14) 7 7) +(+(2 -6) --(-2<=>-3))) (-*(-3 (((*(-(776443==>-4) -3)==>+(1 -*((7<=>-2) 3)))<=>3)==>-13))==>(-2<=>-1))) +(-4 5 (7==>-(3<=>*(((-3<=>(2<=>-2))==>8870307) (*(+(+(5 5 6 7 3 -3 5) 5 -3) -+(--10 12))<=>5))))))

Passed:
(((-(*(2 -3)==>-1)<=>+(2 *(--5 4 -4) (+(7 -3 2)<=>(-*(5 -2)==>4))))<=>*(5 -3 6 7 1 1 -1 3 -3 (-4<=>4)))==>((*(+(1 -1) (*(-1 2 -7)<=>5))==>*(*(((7==>(+(2 1 -6)<=>+(20 3 3 2 3 12 4 4 2 1 3 2 -(4<=>-3) 6 6 19 1 7 2 1 6 1 3 7 1 7 5 20 3 13 1 2 5 5 12 2 3 7 1 4 3  2 3 3 1657260 1 -17)))==>7) 6 --(2==>(3<=>-4))) -5 ((3<=>-2)<=>+(5 8 -6))))==>*((-2==>(-3<=>*(*(-+(2 14 (6==>-5)) +(2 7 2 3 (8==>-2) 3 7 16 3 15) (-2==>(-6==>-2))) -3))) ((1<=>(2==>-1))==>+((5<=>-1) 1 ((1<=>-3)<=>2))) (+(2 -6 -3)<=>(-1<=>*(+(2 4 2 2 -1) 17 3))))))

Passed:
-+(+(-7 +(-1 2 (-4<=>(16==>-1)))) -((*(1 4 4 6 7 6 2 7 -7)<=>-(+(*(1 -1) 2)<=>*(-1 +(-1 17) --3)))<=>(*(2 -6 *(-3 +(---3 *((4==>+(1 -1 -4)) ((-3<=>-3)<=>----3)))))<=>(*((((-1<=>-6)<=>3)<=>-18) -1)<=>-(--11<=>((+(+(+(-4 +(2 -5)) 1 3) *(5 7 1 2 2 -7) --5)<=>(*(-2 -*(13 -4))<=>(4==>((2<=>-5)==>(-1==>+(-1 3 -2))))))==>*(-+(3 *(-1 3 5)) +(-3 (-12==>-1)))))))))

Passed:
+(-*((2==>*(+(3 3 -7) 1 -6)) -2 -((1<=>(-6<=>-2))==>(-2<=>(6<=>(-2<=>*(+(1 1 -5) 1)))))) *(+(-(3==>--2) +(-1 (17<=>-1)) 15) (-3<=>2) *(-(-7<=>(1<=>*(-(-1<=>((-3<=>2)<=>-(2==>-6))) 2 *(3 -1 1 -1 4 (1<=>(3==>-4)) 1 6)))) (*(5 1 *(-4 -2))==>(*((5==>-1) (2<=>-3) *(*(3 4 -8) (3==>(2==>-2))))==>*(*((19<=>-+(-1 11)) (-4<=>--+((-6<=>-7) 4 2))) *(-(3==>-3) (9<=>-2) (19==>-2)) (3<=>(1<=>(-3==>5
)))))))))

+++ OK, passed 100 tests.