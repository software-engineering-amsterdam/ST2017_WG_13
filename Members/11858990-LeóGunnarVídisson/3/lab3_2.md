## Exercise 2 ##
### Assignment ###

- The lecture notes of this week define a function parse for parsing propositional formulas. Test this function. You can use any test method you want.

- Deliverables: test report describing the test method used and the outcome of the test, indication of time spent.

## Completed exercise ##
Here I compare what goes into the parser and what comes out are as expected.
```haskell
parsTesterHalper :: String -> Bool
parsTesterHalper x = (show((parse x)) == addBrack x)
```
Since the parser returns [Form] and I change it to a string, we get an extra set of brackets. Therefore, I add them to the input string when I compare it.
```haskell
addBrack :: String -> String
addBrack x = "["++x++"]"
```
By mapping it I get separated solution for every instance in my array, so I know what input failed.
```haskell
parsTester :: [String] -> [Bool]
parsTester x = map (parsTesterHalper) x
```



### Testing ###

Testing the parser in this implementation Testing the parser is done by throwing valid and none valid items to it.
Then seeing what it return. Then going through it by hand and see if those expected to fail, fail.

```haskell
testInput =[
  (show form2),
  (show form3),
  (show (Equiv form1 form2)),
  (show (Impl form1 form2)),
  (show (Cnj [form1,form2,form3])),
  (show (Dsj[form1,form2,form3])),
  (show (Impl form2 form3)),
  (show (Equiv form2 form3)),
  (show (Cnj [form1,form2,form3])),
  (show (Dsj[form1,form2,form3])),
  "(2 + 3)",
  "",
  "+(2 3",
  "*(2 3)"]
```

## Time and inspiration ##

- Time taken
-- 2 Hours
- reference
-- https://stackoverflow.com/questions/4768927/better-way-to-test-automatically-a-parser

