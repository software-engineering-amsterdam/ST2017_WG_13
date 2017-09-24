# Exercise 4 #
Total time spent 30 minutes

## Specifications of problem ##
To test that the parser works.

## The solution ##
The reflexive nature of parse and show made the testing relatively simple:

````haskell
prop_parsesOk :: Form -> Bool
prop_parsesOk form = [form] == parse ( show form )
````

as we had no way to automatically generate Forms we hard coded some examples:

````haskell
parseTesterInput :: [Form]
parseTesterInput = [

    form1,
    form2,
    form3,
    (Equiv form1 form1),
    (Impl form1 form2),
    (Neg ( Impl form1 form2))
  
  ]
````

and checked that they all worked.

````haskell
parsesAllOk :: [Form] -> Bool
parsesAllOk =  all prop_parsesOk
````

when we completed exercise 4, we circled back and added a QuickCheck test

````haskell
test_parsesOk = verboseCheck prop_parsesOk
````

