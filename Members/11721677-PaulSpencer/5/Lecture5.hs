module Lecture5 where 
import Data.List
import Data.Maybe
import System.Random

type Row         = Int 
type Column      = Int 
type Value       = Int
type Location    = (Row, Column)
type FilledCell  = (Location, Value)
type Grid        = [[Value]]

type ValueAtLocation = Location -> Value
type Constraint = (Location,[Value])
type SnapShot = (ValueAtLocation,[Constraint])


-- run program
main::IO ()
main = do 
  [r] <- rsolveNs [emptyN]
  showSnapShot r
  -- s  <- genProblem r
  -- showSnapShot s



--
showSnapShot::SnapShot -> IO()
showSnapShot (valueFinder, _) = showPotentialGrid valueFinder
    where 
      showPotentialGrid = showGrid . valueFinderToGrid
      valueFinderToGrid valFinder = [[valFinder (r,c) | c <- positions] | r <- positions] 


solveAndShow::Grid -> IO[()]
solveAndShow grid = solveShowSnapShots $ initSnapShot grid
  where 
    solveShowSnapShots sss = (sequence . fmap showSnapShot . solveSnapShots) sss
    solveSnapShots sss = search nextLevelSnapShots novals sss
    nextLevelSnapShots (s,[]) = []
    nextLevelSnapShots (s,p:ps) = branchSnapShot (s,ps) p 
    novals = (\(_,vs) -> null vs)

initSnapShot::Grid -> [SnapShot]
initSnapShot grid = 
  if (not . consistentGrid) valFinder then [] else [(valFinder, getconstraints valFinder)]
    where 
      valFinder = valFinderFromGrid grid
      consistentGrid valFinder = and $ (consisentRows ++ consisentCols ++ consisentSubs)
      consisentRows = map injective $ valsFrom rowlocs
      consisentCols = map injective $ valsFrom collocs
      consisentSubs = map injective $ valsFrom allsubgridlocs
      injective xs = nub xs == xs
      valsFrom locss = map (filter (/= 0)) [[valFinder loc | loc <- locs] | locs <- locss]

branchSnapShot::SnapShot -> Constraint -> [SnapShot]
branchSnapShot (valFinder, constraints) (loc,vs) = [(nextLevelValFinder v, sortPrunedConstraints v) | v <- vs]
  where 
    sortPrunedConstraints val = sortBy remaingValuesCompare $ prune (loc, val) constraints
    nextLevelValFinder val = branchNextLevel valFinder (loc, val)
    branchNextLevel valFinder cell@(loc,val) = update valFinder (loc,val)

update::(Location -> Value) -> (Location, Value) -> Location -> Value 
update valFinder (origloc, val) newloc = 
  if newloc == origloc then 
    val 
  else 
    valFinder newloc 




prune::(Location,Value) -> [Constraint] -> [Constraint]
prune _ [] = []
prune (oloc@(r,c),v) ((cloc@(x,y),zs):rest)
  | r == x                = trimConstraint
  | c == y                = trimConstraint
  | samesubgrid oloc cloc = trimConstraint
  | otherwise             = (cloc,zs) : nextPrune
      where
        trimConstraint = (cloc,zs\\[v]) : nextPrune
        nextPrune = prune (oloc,v) rest
        samesubgrid l1 l2 = or [sg1 == sg2 | sg1 <- subgridsForLoc l1, sg2 <- subgridsForLoc l2]
        
remaingValuesCompare::Constraint -> Constraint -> Ordering
remaingValuesCompare (_,vs) (_,vs') = compare (length vs) (length vs')

getconstraints::ValueAtLocation -> [Constraint] 
getconstraints valFinder = sortedConstraints  
    where 
      sortedConstraints =  sortBy remaingValuesCompare constraintsForLoc
      constraintsForLoc = [(loc, findConstraints valFinder loc) | loc <- openPositions valFinder]
      openPositions valFinder = [(r,c) | r <- positions, c <- positions, valFinder (r,c) == 0]

findConstraints::ValueAtLocation -> Location -> [Value]
findConstraints valFinder loc@(r,c) = notInRow `intersect` notInColumn `intersect` notInSubgrid 
    where 
        notInRow = stillToFind [valFinder loc | loc <- locsofrow r ]
        notInColumn = stillToFind [valFinder loc | loc <- locsofcol c]
        notInSubgrid = stillToFind [valFinder loc' | loc' <- concat $ subgridsForLoc loc]
        stillToFind currentGroup = validvalues \\ currentGroup 
        validvalues = [1..9] 

search::(SnapShot -> [SnapShot]) -> (SnapShot -> Bool) -> [SnapShot] -> [SnapShot]
search children goal [] = []
search children goal (x:xs) 
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

searchBFS::(SnapShot -> [SnapShot]) -> (SnapShot -> Bool) -> [SnapShot] -> [SnapShot]
searchBFS children goal [] = []
searchBFS children goal (x:xs) 
  | goal x    = x : searchBFS children goal xs
  | otherwise = searchBFS children goal (xs ++ (children x))

noremaingvalues ::SnapShot -> Bool
noremaingvalues node = null $ snd node

valFinderFromGrid::Grid -> ValueAtLocation
valFinderFromGrid vs = (\(r,c) -> (vs !! (r-1)) !! (c-1))





emptyN::SnapShot
emptyN = (\ _ -> 0, getconstraints (\ _ -> 0))

getRandomInt::Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem::[a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do 
  n <- getRandomInt maxi
  return [xs !! n]
    where 
      maxi = length xs - 1

sameNrRemaining::Constraint -> Constraint -> Bool
sameNrRemaining (_,xs) (_,ys) = length xs == length ys

getRandomConstraints::[Constraint] -> IO [Constraint]
getRandomConstraints cs = getRandomItem (getConstraints cs) 
  where 
    getConstraints [] = []
    getConstraints (x:xs) = takeWhile (sameNrRemaining x) (x:xs)

rsuccNode::SnapShot -> IO [SnapShot]
rsuccNode (valueFinder,cs) = do 
  xs <- getRandomConstraints cs
  if null xs then return [] else 
    return (branchSnapShot (valueFinder,cs\\xs) (head xs))

rsolveNs::[SnapShot] -> IO [SnapShot]
rsolveNs ns = rsearch rsuccNode noremaingvalues (return ns)

rsearch::(node -> IO [node]) -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = do 
  xs <- ionodes 
  if null xs then return [] else 
    if goal (head xs) then return [head xs] else do 
      ys <- rsearch succ goal (succ (head xs))
      if (not . null) ys then return [head ys] else 
        if null (tail xs) then return [] else 
          rsearch succ goal (return $ tail xs)

-- position helpers
positions::[Int]
positions = [1..9]

stdblocks,nrcblocks::[[Int]]
stdblocks =[[1..3],[4..6],[7..9]]
nrcblocks = [[2..4],[6..8]]

rowlocs, collocs,subgridlocs,allsubgridlocs::[[Location]]
rowlocs = [[(r,c) | c <- positions] | r <- positions]
collocs = [[(r,c) | r <- positions] | c <- positions]
subgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- stdblocks, cs <- stdblocks ]
nrcgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- nrcblocks, cs <- nrcblocks ]
allsubgridlocs = subgridlocs ++ nrcgridlocs

subgridsForLoc::Location -> [[Location]]
subgridsForLoc loc = filter (elem loc) allsubgridlocs

locsofrow::Int -> [Location]
locsofrow row = rowlocs !! (row-1)

locsofcol::Int -> [Location]
locsofcol col = rowlocs !! (col-1)

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
      | c == (last cs)     = colEnd
      | loc `elem` vlocs   = value'
      | loc `elem` corners = corner
      | loc `elem` vlines  = vline
      | loc `elem` hlines  = hline
      | otherwise          = empty
        where
          value' = next valueAtLocation
          valueAtLocation = maybe ' ' head  $ lookup loc $ zip vlocs (map (showVal) (concat vs)) 
          colEnd = next '\n'
          corner = next '+'
          vline = next '|'
          hline = next '-'
          empty = next ' '
          next c = do putChar c ; showGrid' vs ps

-- Examples for testing

example1::Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2::Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3::Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8], 
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4::Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5::Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]



-- create random problem

-- genRandomSudoku::IO SnapShot
-- genRandomSudoku = do [r] <- rsolveNs [emptyN]
--                      return r

-- randomize::Eq a => [a] -> IO [a]
-- randomize xs = do y <- getRandomItem xs 
--                   if null y 
--                     then return []
--                     else do ys <- randomize (xs\\y)
--                             return (head y:ys)
-- randomS = genRandomSudoku >>= showSnapShot

-- uniqueSol::SnapShot -> Bool
-- uniqueSol node = singleton (solveNs [node]) where 
--   singleton [] = False
--   singleton [x] = True
--   singleton (x:y:zs) = False

-- eraseS::ValueAtLocation -> (Row,Column) -> ValueAtLocation
-- eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
--                      | otherwise      = s (x,y)

-- eraseN::SnapShot -> (Row,Column) -> SnapShot
-- eraseN n (r,c) = (s, getconstraints s) 
--   where 
--     s = eraseS (fst n) (r,c) 

-- minimalize::SnapShot -> [(Row,Column)] -> SnapShot
-- minimalize n [] = n
-- minimalize n ((r,c):rcs) 
--   | uniqueSol n' = minimalize n' rcs
--   | otherwise    = minimalize n  rcs
--     where 
--       n' = eraseN n (r,c)

-- filledPositions::ValueAtLocation -> [Location]
-- filledPositions s = [(r,c) | r <- positions, c <- positions, s (r,c) /= 0]

-- genProblem::SnapShot -> IO SnapShot
-- genProblem n = do 
--   ys <- randomize xs
--   return (minimalize n ys)
--     where 
--       xs = filledPositions (fst n)
