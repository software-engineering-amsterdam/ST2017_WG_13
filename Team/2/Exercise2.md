# Exercise 2
~Total time spent: 1.5hrs

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
triangleType :: [Integer] -> Shape
triangleType [a,b,c]
   | not (isTriangle a b c) = NoTriangle
   | isEquilateral a b c = Equilateral
   | isRectangular a b c = Rectangular
   | isIsosceles a b c = Isosceles
   | otherwise = Other
```

The output of the ```triangleType``` function is the shape of the given triangle based on the integer list given. This shape is returned to the original ```triangle``` function as a final output.

#### 3. Testing ####

Testing the correctness of the triangle program is done by testing the order in which inputs are received. This correctness lies in the property that triangles with the same side lengths belong to the same triangle classification, no matter the ordering of the sides (i.e. the triangles rotation along its centerpoint).

To perform this test, the derangement set of the input is used to compare their resulting shape with that of the original input.

```haskell
triangleTest :: Integer -> Integer -> Integer -> Bool
triangleTest a b c = (triangle a b c == triangle b c a) &&
                     (triangle a b c == triangle c a b)
```

A **passed** test shows a differing order of inputs produces equivalent shaped triangles.

A **failed** test shows a differing order of inputs produces inequivalent shaped triangles.

**Test report:**

quickCheck on the triangleTest property passes all tests.

First 10 checks are shown below with the corresponding input:

```haskell
1. 
Passed: 0 0 0

2. 
Passed: 1 -1 -1

3. 
Passed: 0 0 0

4. 
Passed: 1 -1 -2

5. 
Passed: 1 2 -4

6. 
Passed: 0 4 4

7. 
Passed: -3 -6 3

8. 
Passed: -7 -5 -1

9. 
Passed: 4 0 -7

10. 
Passed: -7 -4 0
```




