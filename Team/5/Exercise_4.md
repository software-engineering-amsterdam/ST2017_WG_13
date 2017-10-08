# Exercise 4 #
Total time spent ~3 hours.

## Specifications of problem ##
>Write a program that generates Sudoku problems with three empty blocks. Is it also possible to generate Sudoku problems with four empty blocks? Five? How can you check this?

## the Code

First we select some random blocks and pass back all their locations:

````haskell
blockGen::Int -> IO [[(Row,Column)]]
blockGen n = liftM (map ((!!)sgs')) $ sgcntGen n
  where 
    sgs' = [[(r, c) | r <- rs, c <- cs] | rs <- blocks, cs <- blocks ]
    sgcntGen n = do
      g <- newStdGen
      return (take n . nub $ (randomRs (0,8) g :: [Int]))
````

Next we use the eraseN to erase items from the randomly created blocks.  If this does not have a unique solution then we try again until we get one:

````haskell
eraseblocks::Int -> Node -> IO Node
eraseblocks cnt node = do
  blocks <- blockGen cnt
  if uniqueSol (sol blocks) then return (sol blocks) else eraseblocks cnt node
    where
      sol blks = (eraseblocks' blks)
      eraseblocks' =  foldl (erasecells) node
      erasecells = foldl (eraseN)
````      

Finally, we erase blocks from generated solutions before removing items to get to a minimal solution.

````haskell
main :: Int -> IO ()
main cnt = do 
  [r] <- rsolveNs [emptyN]
  showNode r
  r' <- eraseblocks cnt r
  s  <- genProblem r'
  showNode s
````

## Getting to 4
Thanks to some advice from: https://puzzling.stackexchange.com/questions/309/what-is-the-maximum-number-of-empty-3x3-blocks-a-proper-sudoku-can-have
we were able to limit the search space to the following block combos :[[1,3,5,7],[0,2,4,8],[0,2,4,6],[0,4,6,8],[2,4,6,8]], for a 4 blanck solution and after many runs of the following adapted code we got the following four blank squares:

updated the above code to pick one of: [[1,3,5,7],[0,2,4,8],[0,2,4,6],[0,4,6,8],[2,4,6,8]]
````haskell
main4 :: IO ()
main4 = do 
  [r] <- rsolveNs [emptyN]
  showNode r
  r' <- erase4blocks r
  s  <- genProblem r'
  showNode s

erase4blocks::Node -> IO Node
erase4blocks node = do
  blocks <- block4Gen
  if uniqueSol (sol blocks) then return (sol blocks) else erase4blocks node
    where
      sol blks = (eraseblocks' blks)
      eraseblocks' =  foldl (erasecells) node
      erasecells = foldl (eraseN)

block4Gen::IO [[(Row,Column)]]
block4Gen = liftM (map ((!!)sgs')) $ sgcntGen
  where 
    configs = [[1,3,5,7],[0,2,4,8],[0,2,4,6],[0,4,6,8],[2,4,6,8]]
    sgs' = [[(r, c) | r <- rs, c <- cs] | rs <- blocks, cs <- blocks ]
    sgcntGen = do
      r <- getStdRandom (randomR (0,4))
      return (configs !! r )
````

## best performance: we mangaged to get a 4 blanker:

```     
+-------+-------+-------+
| 4 1 3 | 6 5 7 | 2 9 8 |
| 2 8 9 | 4 3 1 | 5 7 6 |
| 5 6 7 | 9 8 2 | 3 4 1 |
+-------+-------+-------+
| 8 2 5 | 3 6 9 | 7 1 4 |
| 3 7 4 | 1 2 8 | 6 5 9 |
| 6 9 1 | 5 7 4 | 8 2 3 |
+-------+-------+-------+
| 9 5 6 | 7 4 3 | 1 8 2 |
| 1 3 8 | 2 9 5 | 4 6 7 |
| 7 4 2 | 8 1 6 | 9 3 5 |
+-------+-------+-------+
+-------+-------+-------+
| 4     |       | 2     |
| 2 8 9 |       |       |
|   6   |       | 3 4 1 |
+-------+-------+-------+
|       | 3 6   |       |
|       |   2 8 |       |
|       | 5 7 4 |       |
+-------+-------+-------+
|   5   |       |   8   |
| 1 3 8 |       | 4   7 |
| 7 4   |       |   3 5 |
+-------+-------+-------+

```