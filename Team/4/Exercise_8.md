# Exercise 8
~Total time spent: ~30

### Commutativity of Symmetric Closure and Transitive Closure ###

Is there a difference between the symmetric closure of the transitive closure of a relation R and the transitive closure of the symmetric closure of R?

#### Disproving by Counter-Model ####

If we assume that the symmetric closure of the transitive closure of a relation `R` is not equivalent to the transitive closure of the symmetric closure of `R` for all relations, we must construct a counter-model which proves this assumption.

First we take the test relation:
```haskell
testRel = [(0,1)]
```

To find the symmetric closure of the transitive closure of `testRel`, we use the functions constructed in Exercise_5 and Exercise_6 of this lab.

```haskell
x = symClos $ trClos testRel

-- output x = [(0,1),(1,0)]
```

and for the transitive closure of the symmetric closure:
```haskell
y = trClos $ symClos testRel

-- output y = [(0,1),(1,0),(0,0),(1,1)]
```

Viewing the difference between the two realtions `x` and `y`, it is clear they are not equivalent as a non-empty list is returned.

```haskell
diff = y \\ x

-- output diff = [(0,0),(1,1)]
```

It's clear to see that the symmetric closure of the transitive closure over our test relation contains fewer items than the transitive closure of the symmetric closure of our test relation. 

This shows that the properties of the relation are not commutative over each other for all valid relations.

A property for automated testing can be defined as the function:
```haskell
testCommutative :: Rel Int -> Bool
testCommutative r = (symClos $ trClos r) == (trClos $ symClos r)
```


