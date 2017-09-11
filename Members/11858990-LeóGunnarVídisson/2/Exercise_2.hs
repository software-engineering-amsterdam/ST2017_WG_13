-- Recognizing triangles

-- Write a program (in Haskell) that takes a triple of integer values as arguments and gives as output one of 
-- the following statements:
-- * Not a triangle (Geen driehoek) if the three numbers cannot occur as the lengths of the sides of triangle,
-- * Equilateral (Gelijkzijdig) if the three numbers are the lengths of the sides of an equilateral triangle,
-- * Rectangular (Rechthoekig) if the three numbers are the lengths of the sides of a rectangular triangle,
-- * Isosceles (Gelijkbenig) if the three numbers are the lengths of the sides of an isosceles 
--   (but not equilateral) triangle,
-- * Other (Anders) if the three numbers are the lengths of the sides of a triangle that is not equilateral, 
--   not rectangular, and not isosceles.

-- Here is a useful datatype definition:

-- > data Shape = NoTriangle | Equilateral 
-- >            | Isosceles  | Rectangular | Other deriving (Eq,Show)
-- Now define a function triangle :: Integer -> Integer -> Integer -> Shape with the right properties.

-- You may wish to consult wikipedia. Indicate how you tested or checked the correctness of the program.

-- Deliverables: Haskell program, concise test report, indication of time spent.

