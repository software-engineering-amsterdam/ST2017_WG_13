# Exercise 6
~Total time spent: ~ 1 hour 

### Transitive Closure of Relations ###

Use the datatype for relations from the previous exercise, plus
```haskell
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]
```
to define a function
```haskell
 trClos :: Ord a => Rel a -> Rel a 
```

that gives the transitive closure of a relation, represented as an ordered list of pairs.

``` 
Rel           = [(1,2),(2,3),(3,4)]
trClos (Rel)  = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
```

#### 1. Transitive Closure Properties ####

The transitive closure of a relation `R` is the union of `n` compositions of R until R is transitive.
```
trClos R = R U R2 U R3 U .. U R^n
```
E.G.
```
R  = [(1,2),(2,3)]
R2 = [(1,2), (1,3), (2,3)]
R2 is transitive => trClos R = R2
```
#### 2. Program ####

First, a function to check if the relation is transitive (the stop condition):

```haskell
transCheck :: Ord a => Rel a -> Bool
transCheck r = and [elem (x,z) r | (x,y) <- r, (w,z) <- r, y == w]
```

The transitive closure of R is then constructed by the recursing function:

```haskell
trClos :: Ord a => Rel a -> Rel a
trClos r
    | transCheck r = r
    | otherwise = trClos r' where r' = nub (r ++ (r @@ r))
```




