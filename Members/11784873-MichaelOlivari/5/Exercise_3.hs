module Exercise_3

where 

import Data.List
import System.Random
import Lecture5

-- Time Spent: ~2hrs

{- 
-- Too weak, need to check when any of the filled positions are removed not just the first

primeOfNotUnique :: Node -> Bool
primeOfNotUnique n = not (uniqueSol np) where np = eraseN n (filledPositions (fst n) !! 0)
-}

-- subset of sudokus with one filled position less than original
subSuds :: Sudoku -> [Sudoku]
subSuds s = map (\x -> extend s (x, 0)) (filledPositions s)

--Properties of minimal sudoku
-- 1. The sudoku has one unique solution with its given hints
-- 2. All of the sudokus which have one less hint than the original sudoku do not have a unique solution

primeOfNotUnique :: Sudoku -> Bool
primeOfNotUnique s = all (\sp -> not (uniqueSol $ sud2node sp)) $ subSuds s

minimal :: Node -> Bool
minimal n = uniqueSol n && primeOfNotUnique (fst n)

--help functions
grid2node :: Grid -> Node
grid2node g = sud2node $ grid2sud g

sud2node :: Sudoku -> Node
sud2node s = (s, constraints s)

--- Testing
testMin :: Grid -> Bool
testMin g = minimal $ grid2node g

exampleTest = testMin example1 :
              testMin example2 :
              testMin example3 :
              testMin example4 :
              testMin example5 : []

-- Result: [False,False,True,False,False]

testRandom :: IO Bool
testRandom = do [r] <- rsolveNs [emptyN]
                s <- genProblem r
                showNode s
                showNode r
                return $ minimal s

{-
Random Sudoku:
+-------+-------+-------+
|     7 |     3 | 6     |
|     4 |     9 | 3 2   |
| 1   6 |       |       |
+-------+-------+-------+
|   7 3 |   1 4 |       |
| 5 4   | 8     |       |
|       | 9     |     2 |
+-------+-------+-------+
|     2 | 4   8 | 9     |
|     5 |       |       |
| 3     |   2   | 8     |
+-------+-------+-------+

Solution:
+-------+-------+-------+
| 9 2 7 | 5 4 3 | 6 1 8 |
| 8 5 4 | 1 6 9 | 3 2 7 |
| 1 3 6 | 2 8 7 | 4 9 5 |
+-------+-------+-------+
| 2 7 3 | 6 1 4 | 5 8 9 |
| 5 4 9 | 8 7 2 | 1 6 3 |
| 6 1 8 | 9 3 5 | 7 4 2 |
+-------+-------+-------+
| 7 6 2 | 4 5 8 | 9 3 1 |
| 4 8 5 | 3 9 1 | 2 7 6 |
| 3 9 1 | 7 2 6 | 8 5 4 |
+-------+-------+-------+

Minimal? :
True
-}
