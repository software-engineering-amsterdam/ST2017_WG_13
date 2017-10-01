# Exercise 5
~Total time spent: 30mins including necessary readings

### Symmetric Closure of Relations ###


Suppose we implement binary relations as list of pairs, Haskell type [(a,a)]. Assume the following definition:

```haskell 
type Rel a = [(a,a)] 
```

Use this to implement a function

```haskell
symClos :: Ord a => Rel a -> Rel a
```

That gives the symmetric closure of a relation, where the relation is represented as an ordered list of pairs.

``` 
Rel           = [(1,2),(2,3),(3,4)]
symClos (Rel) = [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
```

#### 1. Symmetric Closure Properties ####

The symmetric closure of a relation `R` is the union of the relation and its inverse: `S = R U R-`

E.G.
```
R  = [(1,2),(2,3)]
R- = [(2,1),(3,2)]
S  = [(1,2),(2,1),(2,3),(3,2)]
```
#### 2. Program ####

First, a function to find the inverse of a relation R:

```haskell
inverseR :: Rel a -> Rel a
inverseR [] = []
inverseR ((x,y):xs) = [(y,x)] ++ inverseR xs
```

The symmetric closure of R is then constructed by the function:

```haskell
symClos :: Ord a => Rel a -> Rel a
symClos r = nub (r ++ (inverseR r))
```




