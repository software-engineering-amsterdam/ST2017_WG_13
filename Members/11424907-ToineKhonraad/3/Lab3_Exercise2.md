## Exercise 1 ##

### Task ###

The task is to give definitions of:

**Contradiction** 

```contradiction :: Form -> Bool```

**Tautology**

```tautology :: Form -> Bool```

**Logical entailment**

```entails :: Form -> Form -> Bool```

**Logical equivalence**

```equiv :: Form -> Form -> Bool```

### Assignment ###

- Check that your definitions are correct.
- Deliverables: implementation, description of your method of checking the definitions, indication of time spent.

## Completed exercise ##

For expressiveness I defined the function **none** as

```haskell
none :: Foldable t => (a -> Bool) -> t a -> Bool
none p = not . (any p)
```

The lecture notes give a definition of satisfiable, for objects of type Form:

**Satisfiable** 

```haskell
satisfiable :: Form -> Bool
satisfiable f = any (\ v -> evl v f) (allVals f)
```

*A formula is satisfiable, if there exists a valuation in all possible valuations of its members for which the formula is True.*

**Contradiction** 

```haskell
contradiction :: Form -> Bool
contradiction f = not $ satisfiable f
```

*If a formula is a contradiction then in all possible valuations, there exist no value such that the formula is True, in other words, it is a formula that is not satisfiable.*

**Tautology**

```Haskell
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)
```

*A formula is a tautology, if there does not exist a valuation in all possible valuations of its members for which the formula is False; hence it is True for all valuations.*

**Logical entailment**

```haskell
entails :: Form -> Form -> Bool

TBC

```

> From sthe sheets of the lecture:
> 
> *B logically entails A is true if and only if it is necessary that if all of the elements of B are true, then A is true.*
> 

**Logical equivalence**

```haskell
equiv :: Form -> Form -> Bool

TBC
```


