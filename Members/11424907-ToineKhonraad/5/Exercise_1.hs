module Exercise_1 where 
import Data.List

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

--- Display Grid
showVal::Value -> String
showVal 0 = " "
showVal d = show d

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

-- position helpers
positions::[Int]
positions = [1..9]

stdblocks,nrcblocks::[[Int]]
stdblocks =[[1..3],[4..6],[7..9]]
nrcblocks = [[2..4],[6..8]]

type Location = (Row, Column)

alllocs::[Location]
alllocs = [(r,c) | r <- positions, c <- positions]

rowlocs, collocs,subgridlocs,nrcgridlocs::[[Location]]
rowlocs = [[(r,c) | c <- positions] | r <- positions]
collocs = [[(r,c) | r <- positions] | c <- positions]
subgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- stdblocks, cs <- stdblocks ]
nrcgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- nrcblocks, cs <- nrcblocks ]

--
type Sudoku = Location -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = map (map s) rowlocs 

grid2sud :: Grid -> Sudoku
grid2sud gr = pos gr
  where 
  pos :: Grid -> Location -> Value 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

sublocs4loc, nrclocs4loc:: Location -> [Location]
sublocs4loc loc = sglocs4loc loc subgridlocs
nrclocs4loc loc = sglocs4loc loc nrcgridlocs

sglocs4loc::Location -> [[Location]] -> [Location]
sglocs4loc loc slocs = concat $ filter (elem loc) slocs

subGrid, nrcGrid :: Sudoku -> Location -> [Value]
subGrid s loc =  [s loc' | loc' <- sublocs4loc loc]
nrcGrid s loc = [s loc' | loc' <- nrclocs4loc loc]

freeInSeq :: [Value] -> [Value]
freeInSeq seq = [1..9] \\ seq 

rowValues,freeInRow::Sudoku -> Row -> [Value]
rowValues s r = [s loc | loc <- rowlocs !! (r-1)]
freeInRow s r = freeInSeq $ rowValues s r

columnValues,freeInColumn::Sudoku -> Column -> [Value]
columnValues s c = [s loc | loc <- collocs !! (c-1)]
freeInColumn s c = freeInSeq $ columnValues s c

freeInSubgrid, freeInNrcgrid :: Sudoku -> Location -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))
freeInNrcgrid s (r,c) = freeInSeq (nrcGrid s (r,c))

freeAtPos :: Sudoku -> Location -> [Value]
freeAtPos s loc@(r,c) = 
  freeInRow s r 
   `intersect` freeInColumn s c 
   `intersect` freeInSubgrid s loc 
   `intersect` freeInNrcgrid s loc 

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective,colInjective,subgridInjective, nrcgridInjective :: Sudoku -> Location -> Bool
rowInjective s (r,_) = injective $ gone $ rowValues s r
colInjective s (_,c) = injective $ gone $ columnValues s c
subgridInjective s loc = injective $ gone $ subGrid s loc
nrcgridInjective s loc = injective $ gone $ nrcGrid s loc

gone::[Value] -> [Value]
gone = filter (/= 0)

consistent :: Sudoku -> Bool
consistent s = and $ consistentRow ++ consistentCol ++ consistentSub ++ consistentNrc
  where 
    consistentRow = isInjective rowInjective rowlocs
    consistentCol = isInjective colInjective collocs
    consistentSub = isInjective subgridInjective subgridlocs
    consistentNrc = isInjective nrcgridInjective nrcgridlocs
    isInjective inj locs =  [inj s loc | loc <- map head locs ]

extend :: Sudoku -> (Location,Value) -> Sudoku
extend s (loc,v) loc' 
  | loc == loc' = v
  | otherwise   = s loc'

type Constraint = (Location,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode (s,_) = showSudoku s

solved  :: Node -> Bool
solved (_,c) = null c

extendNode :: Node -> Constraint -> [Node]
extendNode (s,cs) (loc,vs) = [(extend s (loc,v), sortBy valLen $ prune (loc,v) cs) | v <- vs ]

valLen :: (Location,[Value]) -> (Location,[Value]) -> Ordering
valLen (_,vs) (_,vs') = compare (length vs) (length vs')

sameblock :: Location -> Location -> Bool
sameblock lx ly = sublocs4loc lx == sublocs4loc ly 

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

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]

openPositions :: Sudoku -> [Location]
openPositions s = [ loc | loc <- alllocs, s loc == 0 ]

constraints :: Sudoku -> [Constraint] 
constraints s = sortBy valLen [(loc, freeAtPos s loc) | loc <- openPositions s ]

search :: (Node -> [Node]) -> (Node -> Bool) -> [Node] -> [Node]
search getChild goal [] = []
search getChild goal (x:xs) 
  | goal x    = x : search getChild goal xs
  | otherwise = search getChild goal (getChild x ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search childNode solved 

childNode :: Node -> [Node]
childNode (s,[]) = []
childNode (s,p:ps) = extendNode (s,ps) p 

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs ns = traverse showNode (solveNs ns)

exampleNrc :: Grid 
exampleNrc = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]]

solveNrc = solveAndShow exampleNrc
nrc1000 = redoAction 1000 solveNrc

--redoAction :: Int -> IO () -> IO ()
redoAction n action
    | n <= 0    = return () 
    | otherwise = do action  
                     redoAction (n-1) action
