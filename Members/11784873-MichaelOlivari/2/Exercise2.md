# Exercise 2
~ Total time spent: 45 mins

### Recognizing Triangles ###

Write a program (in Haskell) that takes a triple of integer values as arguments and gives as output one of the following statements:

* Not a triangle (Geen driehoek) if the three numbers cannot occur as the lengths of the sides of triangle,

* Equilateral (Gelijkzijdig) if the three numbers are the lengths of the sides of an equilateral triangle,

* Rectangular (Rechthoekig) if the three numbers are the lengths of the sides of a rectangular triangle,

* Isosceles (Gelijkbenig) if the three numbers are the lengths of the sides of an isosceles (but not equilateral) triangle,

* Other (Anders) if the three numbers are the lengths of the sides of a triangle that is not equilateral, not rectangular, and not isosceles.

Here is a useful datatype definition:
```haskell
> data Shape = NoTriangle | Equilateral 
>            | Isosceles  | Rectangular | Other deriving (Eq,Show)
```
Now define a function ```triangle :: Integer -> Integer -> Integer -> Shape ``` with the right properties.

You may wish to consult wikipedia. Indicate how you tested or checked the correctness of the program.

Deliverables: Haskell program, concise test report, indication of time spent.

#### 1. Triangle Properties ####

A triangle is defined by the sum of the smaller sides being greater than the largest side.

```haskell
isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = ((a /= 0) && (b /= 0) && (c /= 0)) && ((a + b) > c)
```

Equilateral Triangle: All sides of triangle are same length
```haskell
isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = a == b && b == c
```

Isosceles Triangle: Two sides are the same length
```haskell
isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c = (a == b) || (b == c) 
```

Rectangular / Right Triangle: Follows Pythagorean Theorem
```haskell
isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = (a^2 + b^2) == c^2
```

#### 2. Program ####

To begin with, we take the 3 integers provided as input for the ```triangle``` function and sort them from smallest to largest. This makes property checking much easier.

```haskell
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = triangleType (sort [a,b,c])
```

The sorted integer list is then passed as input for the ```triangleType``` function which will classify the the integer list into a specific triangle shape based on the triangle properties specified above.

```haskell