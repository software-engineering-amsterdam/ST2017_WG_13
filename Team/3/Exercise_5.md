## Exercise 5 (Bonus) ##

### Task ###

Write a program for converting formulas to clause form. You may assume that your formulas are already in CNF. Here is the appropriate declaration:

```
cnf2cls :: Form -> Clauses
```

If you combine your conversion function from an earlier exercise with cnf2cls you have a function that can convert any formula to clause form.

Use automated testing to check whether your translation is correct, employing some appropriate properties to check.

### Deliverables ###
 
- Conversion program, 
- test generator, 
- test properties, 
- documentation of the automated testing process. 
- Also, give an indication of time spent.

## Completed exercise ##

The conversion program is defined in the file `Exercise_3.hs`.

As a test generator, we used the offspring of exercise 4 and fed that into the CNF-convertor of Exercise 3. The property to test with Quickcheck, could then be formulated as:

```
prop_equivalent :: Form -> Bool
prop_equivalent form = (formToCnf form)
                       `equiv`
                       (cls2form $ cnf2cls $ formToCnf form)
```

## Time ##

Time taken 2,5 hours.

