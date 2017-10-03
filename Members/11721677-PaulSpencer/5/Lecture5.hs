module Lecture5 where 
import Data.List
import Data.Maybe
import System.Random

type Row         = Int 
type Column      = Int 
type Value       = Int
type Location    = (Row, Column)
type SolvedCell  = (Location, Value)
type Grid        = [[Value]]

type ValueAtLocation = Location -> Value
type Constraint = (Location,[Value])
type SnapShot = (ValueAtLocation,[Constraint])

-- run program
main::IO ()
main = do 
  [randomSnapShot] <- randomSolveSnapshot [emptySnapShot]
  showSnapShot randomSnapShot
  snapShot  <- genProblem randomSnapShot
  showSnapShot snapShot


-- create empty Grid
emptySnapShot::SnapShot
emptySnapShot = (\_ -> 0, getconstraints (\_ -> 0))

getconstraints::ValueAtLocation -> [Constraint] 
getconstraints valFinder = sortedConstraints  
    where 
      sortedConstraints =  sortBy possibilitiesOrd constraintsForLoc
      constraintsForLoc = [makeConstraint loc | loc <- openPositions]
      makeConstraint loc = (loc, findRemaining valFinder loc)
      openPositions = [(r,c) | r <- positions, c <- positions, valFinder (r,c) == 0]

possibilitiesOrd::Constraint -> Constraint -> Ordering
possibilitiesOrd (_,vs) (_,vs') = compare (length vs) (length vs')

findRemaining::ValueAtLocation -> Location -> [Value]
findRemaining valFinder loc@(r,c) = notInRow `intersect` notInColumn `intersect` notInSubgrid 
    where 
        notInRow = stillToFind [valFinder loc | loc <- locsInRow r ]
        notInColumn = stillToFind [valFinder loc | loc <- locsInCol c]
        notInSubgrid = stillToFind [valFinder loc' | loc' <- concat $ subgridsForLoc loc]
        stillToFind currentGroup = validvalues \\ currentGroup 
        validvalues = [1..9]


-- fill Grid
randomSolveSnapshot::[SnapShot] -> IO [SnapShot]
randomSolveSnapshot snapShot = snapShotSearch randomNextSnapShot noPossibilities (return snapShot)
  where
    noPossibilities = (\(_,vs) -> null vs)
    randomNextSnapShot (valueFinder,cs) = do 
      xs <- getRandomConstraints cs
      if null xs then return [] else return (branchSnapShot (valueFinder,cs\\xs) (head xs))

getRandomConstraints::[Constraint] -> IO [Constraint]
getRandomConstraints cs = getRandomItem (getConstraints cs) 
  where 
    getConstraints [] = []
    getConstraints constraints@(x:_) = takeWhile (sameNrRemaining x) constraints
    sameNrRemaining (_,xs) (_,ys) = length xs == length ys

getRandomItem::[a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do 
  index <- getStdRandom $ randomR (0,(length xs - 1))
  return [xs !! index]

snapShotSearch::(SnapShot -> IO [SnapShot]) -> (SnapShot -> Bool) -> IO [SnapShot] -> IO [SnapShot]
snapShotSearch nextSnapShot predicate snapShotsIO = do 
  snapShots <- snapShotsIO 
  nextSnapShots <- nextSnapShotsIO snapShots
  handleSnapShot snapShots nextSnapShots
    where
      nothing = return []
      nextSnapShotsIO ss = if null ss then nothing else snapShotSearch nextSnapShot predicate (nextSnapShot $ head ss)
      handleSnapShot ss nxtss
        | null ss             = nothing
        | predicate (head ss) = return [head ss]
        | (not . null) nxtss  = return [head nxtss]
        | null (tail ss)      = nothing
        | otherwise           = snapShotSearch nextSnapShot predicate (return $ tail ss)

branchSnapShot::SnapShot -> Constraint -> [SnapShot]
branchSnapShot (valFinder, constraints) (loc,vs) = [(getvalfinder v, sortPrunedConstraints v) | v <- vs]
  where 
    getvalfinder val newloc = if newloc == loc then val else valFinder newloc
    sortPrunedConstraints val = sortBy possibilitiesOrd $ reducePossibilities (loc, val) constraints

reducePossibilities::SolvedCell -> [Constraint] -> [Constraint]
reducePossibilities _ [] = []
reducePossibilities solvedCell@(oloc,v) (constraint@(cloc,vs):constraints)
  | shouldReduce oloc cloc = trimPossibilities : nextReduction
  | otherwise            = constraint : nextReduction
      where
        trimPossibilities = (cloc,vs\\[v])
        nextReduction = reducePossibilities solvedCell constraints
        shouldReduce ol@(r, c) cl@(r',c') = inSameRow || inSameCol || inSameSubgrid
          where
            inSameRow = r == r'
            inSameCol = c == c' 
            inSameSubgrid = or [sg1 == sg2 | sg1 <- subgridsForLoc ol, sg2 <- subgridsForLoc cl]

showSnapShot::SnapShot -> IO()
showSnapShot (valFinder, _) = showGrid $ map (map valFinder) collocs  

-- remove items from Grid
genProblem::SnapShot -> IO SnapShot
genProblem snapShot@(valFinder,_) = do 
  ys <- randomizeLocations solvedLocations
  return (minimalize snapShot ys)
  where 
    solvedLocations = [loc | loc <- concat $ rowlocs, valFinder loc /= 0]

randomizeLocations:: [Location] -> IO [Location]
randomizeLocations locs = do 
  rloc <- getRandomItem locs 
  if null rloc then 
    return []
  else do 
    rlocs <- randomizeLocations (locs\\rloc)
    return (head rloc:rlocs)

minimalize::SnapShot -> [Location] -> SnapShot
minimalize snapShot [] = snapShot
minimalize snapShot@(valFinder,_) (loc:locs) 
  | uniqueSolution snapShot' = minimalize snapShot' locs
  | otherwise                = minimalize snapShot  locs
    where 
      snapShot' = removeSnapShot  
      removeSnapShot = (s, getconstraints s) 
        where 
          s = eraseS valFinder loc 

uniqueSolution = undefined


eraseS::ValueAtLocation -> (Row,Column) -> ValueAtLocation
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)


-- uniqueSolution::SnapShot -> Bool
-- uniqueSolution node = singleton (solveNs [node]) where 
--   singleton [] = False
--   singleton [x] = True
--   singleton (x:y:zs) = False



-- create random problem

-- genRandomSudoku::IO SnapShot
-- genRandomSudoku = do [r] <- randomSolveSnapShot [emptySnapShot]
--                      return r


-- randomS = genRandomSudoku >>= showSnapShot











-- solveAndShow::Grid -> IO[()]
-- solveAndShow grid = solveShowSnapShots $ initSnapShot grid
--   where 
--     solveShowSnapShots sss = (sequence . fmap showSnapShot . solveSnapShots) sss
--     solveSnapShots sss = search nextLevelSnapShots noVals sss
--     nextLevelSnapShots (s,[]) = []
--     nextLevelSnapShots (s,p:ps) = branchSnapShot (s,ps) p 
--     noVals = (\(_,vs) -> null vs)

-- initSnapShot::Grid -> [SnapShot]
-- initSnapShot grid = 
--   if (not . consistentGrid) valFinder then [] else [(valFinder, getconstraints valFinder)]
--     where 
--       valFinder = valFinderFromGrid grid
--       consistentGrid valFinder = and $ (consisentRows ++ consisentCols ++ consisentSubs)
--       consisentRows = map injective $ valsFrom rowlocs
--       consisentCols = map injective $ valsFrom collocs
--       consisentSubs = map injective $ valsFrom allsubgridlocs
--       injective xs = nub xs == xs
--       valsFrom locss = map (filter (/= 0)) [[valFinder loc | loc <- locs] | locs <- locss]

        
-- valFinderFromGrid::Grid -> ValueAtLocation
-- valFinderFromGrid vs = (\(r,c) -> (vs !! (r-1)) !! (c-1))

-- search::(SnapShot -> [SnapShot]) -> (SnapShot -> Bool) -> [SnapShot] -> [SnapShot]
-- search children goal [] = []
-- search children goal (x:xs) 
--   | goal x    = x : search children goal xs
--   | otherwise = search children goal ((children x) ++ xs)


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

locsInRow::Int -> [Location]
locsInRow row = rowlocs !! (row-1)

locsInCol::Int -> [Location]
locsInCol col = collocs !! (col-1)

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

