# Exercise 4 #
Total time spent ~3 hours.

## Specifications of problem ##
>Write a program that generates Sudoku problems with three empty blocks. Is it also possible to generate Sudoku problems with four empty blocks? Five? How can you check this?

## best performance: we mangaged to get a 4 blanker:


     
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

## the Code

````haskell
main :: Int -> IO ()
main cnt = do 
  [r] <- rsolveNs [emptyN]
  showNode r
  r' <- eraseblocks cnt r
  s  <- genProblem r'
  showNode s

eraseblocks::Int -> Node -> IO Node
eraseblocks cnt node = do
  blocks <- blockGen cnt
  if uniqueSol (sol blocks) then return (sol blocks) else eraseblocks cnt node
    where
      sol blks = (eraseblocks' blks)
      eraseblocks' =  foldl (erasecells) node
      erasecells = foldl (eraseN)
      
blockGen::Int -> IO [[(Row,Column)]]
blockGen n = liftM (map ((!!)sgs')) $ sgcntGen n
  where 
    sgs' = [[(r, c) | r <- rs, c <- cs] | rs <- blocks, cs <- blocks ]
    sgcntGen n = do
      g <- newStdGen
      return (take n . nub $ (randomRs (0,8) g :: [Int]))

````

thanks to some advice from: https://puzzling.stackexchange.com/questions/309/what-is-the-maximum-number-of-empty-3x3-blocks-a-proper-sudoku-can-have
we were able to limit the search space for a 4 blanck solution and after many runs of the following adapted code we got the following four blank squares:




## Timing

This is relevant for Q2, when I set my ghci settings with `:set +s` and ran this 1000 times with the following code we got the following time/size:  (5.11 secs, 508,649,672 bytes)

```` haskell
solveNrc = solveAndShow exampleNrc

redoAction n action
  | n <= 0    = return () 
  | otherwise = do 
    action  
    redoAction (n-1) action

nrc1000 = redoAction 1000 solveNrc
````

## Program ##

### nrc updates

first we added new block to describe the nrc blocks ang generate locations:

```` haskell
stdblocks,nrcblocks::[[Int]]
stdblocks =[[1..3],[4..6],[7..9]]
nrcblocks = [[2..4],[6..8]]

nrcgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- nrcblocks, cs <- nrcblocks ]
```

Next update finding grid locations and values for the nrc grid:

````haskell
nrclocs4loc:: Location -> [Location]
nrclocs4loc loc = sglocs4loc loc nrcgridlocs

nrcGrid :: Sudoku -> Location -> [Value]
nrcGrid s loc = [s loc' | loc' <- nrclocs4loc loc]
````

next Find remaining available numbers in nrc grid
````haskell
freeInNrcgrid :: Sudoku -> Location -> [Value]
freeInNrcgrid s (r,c) = freeInSeq (nrcGrid s (r,c))
````

Next update the consistency checker
````haskell
nrcgridInjective :: Sudoku -> Location -> Bool
nrcgridInjective s loc = injective $ gone $ nrcGrid s loc

consistent :: Sudoku -> Bool
consistent s = and $ consistentRow ++ consistentCol ++ consistentSub ++ consistentNrc
  where 
    consistentRow = isInjective rowInjective rowlocs
    consistentCol = isInjective colInjective collocs
    consistentSub = isInjective subgridInjective subgridlocs
    consistentNrc = isInjective nrcgridInjective nrcgridlocs
    isInjective inj locs =  [inj s loc | loc <- map head locs ]
````

Finally, update the pruner to check

````haskell
sameNrcblock :: Location -> Location -> Bool
sameNrcblock lx ly = (not. null $ nrclocs4loc lx) && (nrclocs4loc lx == nrclocs4loc ly)

prune :: (Location,Value) -> [Constraint] -> [Constraint]
prune _ [] = []
prune (lx@(r,c),v) ((ly@(x,y),vs):rest)
  | connected = trim : next
  | otherwise = (ly,vs) : next
  where
    connected = (r == x) || (c == y) || sameblock lx ly || sameNrcblock lx ly
    trim = (ly,vs\\[v])
    next = prune (lx,v) rest
````


### Unnecessary changes
For readability, naming and formatting changes were added.  We also converged on Location that is similar to Position in Q2.  code that was not necessary for this question was removed.  location helpers were added:

````haskell
type Location = (Row, Column)

alllocs::[Location]
alllocs = [(r,c) | r <- positions, c <- positions]

rowlocs, collocs,subgridlocs,nrcgridlocs::[[Location]]
rowlocs = [[(r,c) | c <- positions] | r <- positions]
collocs = [[(r,c) | r <- positions] | c <- positions]
subgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- stdblocks, cs <- stdblocks ]
nrcgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- nrcblocks, cs <- nrcblocks ]
````

completely unnecessarily I update the showGrid, to make it simpler to use for me. (this can be ignored)

````haskell
type DrawLoc = (Int, Int)
cs = [0..35]
rs = [0..18]

vlocs = [(r,c) | r <- rs, c <- [2,6,9,13,17,21,25,28,32], odd r]
mainGridHLines = [0,6,12,18]
mainGridVLines = [0,11,23,34]
nrcGridHLines = [[2,8],[10,16]]
nrcGridVLines = [[4,15],[19,30]]

corners,stdcorners,nrccorners::[DrawLoc]
corners = stdcorners ++ nrccorners
stdcorners = [(r,c) | r <- mainGridHLines, c <- mainGridVLines]
nrccorners = [(r,c) | r <- concat nrcGridHLines, c <- concat nrcGridVLines]

vlines,stdvlines,nrcvlines::[DrawLoc]
vlines = stdvlines ++ nrcvlines
stdvlines = [(r,c) | r <- rs, c <- mainGridVLines]
nrcvlines = [(r,c) | r <- [2..8]++[10..16], c <- concat nrcGridVLines]

hlines,stdhlines,nrchlines::[DrawLoc]
hlines = stdhlines ++ nrchlines
stdhlines = [(r,c) | r <- mainGridHLines, c <- cs]
nrchlines = [(r,c) | r <- concat nrcGridHLines, c <- [4..15]++[19..30]]

drawingPositions::[DrawLoc]
drawingPositions = [(r, c) | r <- rs, c <- cs]
    
showGrid::Grid -> IO ()
showGrid vs = showGrid' vs drawingPositions 
  where 
    showGrid' _ [] = putStrLn "" 
    showGrid' vs (loc@(r,c) : ps)
      | c == last cs       = colEnd
      | loc `elem` vlocs   = value'
      | loc `elem` corners = corner
      | loc `elem` vlines  = vline
      | loc `elem` hlines  = hline
      | otherwise          = empty
        where
          value' = next valueAtLocation
          valueAtLocation = maybe ' ' head  $ lookup loc $ zip vlocs (map showVal (concat vs)) 
          colEnd = next '\n'
          corner = next '+'
          vline = next '|'
          hline = next '-'
          empty = next ' '
          next c = do putChar c ; showGrid' vs ps

````
