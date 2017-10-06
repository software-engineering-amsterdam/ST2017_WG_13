module Lecture5 where 
import Data.List
import Data.Maybe
import System.Random
import Debug.Trace
import Control.Applicative

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
  [randomfilledGrid] <- randomSolveSnapshot [emptySnapShot]
  showSnapShot randomfilledGrid
  problemSnapShot  <- trace ("***************************************\n") (genProblem randomfilledGrid)
  showSnapShot problemSnapShot

-- create empty Grid
emptySnapShot::SnapShot
emptySnapShot = (\_ -> 0, getconstraints (\_ -> 0))

getconstraints::ValueAtLocation -> [Constraint] 
getconstraints valFinder =  (sortByConstraintVals allConstraints) 
    where 
      allConstraints = [makeConstraint loc | loc <- openPositions]
      openPositions = [loc | loc <- alllocs, valFinder loc == 0]
      makeConstraint loc = (loc, getConstraintValues valFinder loc)


sortByConstraintVals::[Constraint] -> [Constraint] 
sortByConstraintVals =  (sortBy (\(_,c1) (_,c2) -> compare (length c1) (length $ c2)))

getConstraintValues::ValueAtLocation -> Location -> [Value]
getConstraintValues valFinder loc@(r,c) = (wipeOutExisting)
    where 
        wipeOutExisting = notInRow `intersect` notInColumn `intersect` notInSubgrid 
        notInRow = stillToFind [valFinder loc | loc <- locsInRow r ]
        notInColumn = stillToFind [valFinder loc | loc <- locsInCol c]
        notInSubgrid = stillToFind [valFinder loc' | loc' <- concat $ subgridsForLoc loc]
        stillToFind currentGroup = validvalues \\ currentGroup 
        validvalues = [1..9]


-- fill Grid
randomSolveSnapshot::[SnapShot] -> IO [SnapShot]
randomSolveSnapshot snapShot = (snapShotSearch randomNextSnapShot noPossibilities (return snapShot))
  where
    noPossibilities = (\(_,vs) -> null vs)
    randomNextSnapShot (valFinder,cs) = do 
      xs <- getRandomConstraints cs
      if null xs then return [] else  (return (branchSnapShot (valFinder,cs\\xs) (head xs)))

getRandomConstraints::[Constraint] -> IO [Constraint]
getRandomConstraints cs =  (getRandomItem (getsamesizedconstraints cs)) 
  where 
    getsamesizedconstraints [] = []
    getsamesizedconstraints constraints@(x:_) = takeWhile (sameNrRemaining x) constraints
    sameNrRemaining (_,xs) (_,ys) = length xs == length ys

getRandomItem::[a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do 
  index <- getStdRandom $ randomR (0,(length xs - 1))
  return [xs !! index]

snapShotSearch::(SnapShot -> IO [SnapShot]) -> (SnapShot -> Bool) -> IO [SnapShot] -> IO [SnapShot]
snapShotSearch nextSnapShot predicate snapShotsIO = (do 
  snapShots <- snapShotsIO 
  nextSnapShots <- nextSnapShotsIO snapShots
  handleSnapShot snapShots nextSnapShots)
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
branchSnapShot (valFinder, constraints) (loc,vs) = ([(getvalfinder v, sortedConstraints v) | v <- vs])
  where 
    getvalfinder val newloc = (if newloc == loc then val else valFinder newloc)
    sortedConstraints val = sortByConstraintVals $ reducePossibilities (loc, val) constraints
--    sortedConstraints val = sortByConstraintVals $ cleanconstraints $ reducePossibilities (loc, val) constraints


reducePossibilities::SolvedCell -> [Constraint] -> [Constraint]
reducePossibilities _ [] = []
reducePossibilities solvedCell@(oloc,v) (constraint@(cloc,vs):constraints)
  | shouldReduce oloc cloc = trimPossibilities : nextReduction
  | otherwise              = constraint : nextReduction
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
genProblem snapShot@(valFinder,_) = (do 
  ys <- randomizeLocations solvedLocations
  return (minimalize snapShot ys))
  where 
    solvedLocations = [loc | loc <- alllocs, valFinder loc /= 0]

randomizeLocations:: [Location] -> IO [Location]
randomizeLocations locs = do 
  rloc <- getRandomItem locs 
  if null rloc then 
    return []
  else do 
    rlocs <- randomizeLocations (locs\\rloc)
    return (head rloc:rlocs)



    -- uniqueSol :: Node -> Bool
    -- uniqueSol node = singleton (solveNs [node]) where 
    --   singleton [] = False
    --   singleton [x] = True
    --   singleton (x:y:zs) = False
    
    -- eraseS :: Sudoku -> (Row,Column) -> Sudoku
    -- eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
    --                      | otherwise      = s (x,y)
    
    -- eraseN :: Node -> (Row,Column) -> Node
    -- eraseN n (r,c) = (s, constraints s) 
    --   where s = eraseS (fst n) (r,c) 
    
    -- minimalize :: Node -> [(Row,Column)] -> Node
    -- minimalize n [] = n
    -- minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
    --                          | otherwise    = minimalize n  rcs
    --   where n' = eraseN n (r,c)
        

minimalize::SnapShot -> [Location] -> SnapShot
minimalize snapShot [] = snapShot
minimalize snapShot@(valFinder,_) (loc:locs) 
  | uniqueSolution snapShot' = minimalize snapShot' locs
  | otherwise                = minimalize snapShot  locs
    where 
      snapShot' = (newValFinder, getconstraints newValFinder) 
      newValFinder = (\newloc -> if loc == newloc then 0 else valFinder newloc)

-- uniqueSolution::SnapShot -> Bool
-- uniqueSolution snapShot = trace ("^v^v^v^v^")  (length (solveSnapShots [snapShot]) == 1)

uniqueSolution :: SnapShot -> Bool
uniqueSolution node = singleton (solveSnapShots [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

solveSnapShots::[SnapShot] -> [SnapShot]
--solveSnapShots sss = trace ("^^^^^^^^^^^^^^^") (searchDFS nextLevelSnapShots noVals sss)
solveSnapShots sss =  (searchDFS nextLevelSnapShots noVals sss)
    where 
      nextLevelSnapShots (_,[]) = []
--      nextLevelSnapShots (valFinder,p:ps) = trace("vv") (branchSnapShot (valFinder,ps) p) 
      nextLevelSnapShots (valFinder,p:ps) =  (branchSnapShot (valFinder,ps) p) 
      noVals = (\(_,vs) -> null vs)


searchDFS::(SnapShot -> [SnapShot]) -> (SnapShot -> Bool) -> [SnapShot] -> [SnapShot]
searchDFS children goal [] = []
searchDFS children goal (x:xs) 
  | goal x    = x : searchDFS children goal xs
  | otherwise = searchDFS children goal ((children x) ++ xs)
  
--type Constraint = (Location,[Value])

searchBFS::(SnapShot -> [SnapShot]) -> (SnapShot -> Bool) -> [SnapShot] -> [SnapShot]
searchBFS children goal [] = []
searchBFS children goal (x:xs) 
  | goal x    = x : searchBFS children goal xs
  | otherwise = searchBFS children goal ((children x) ++ xs)

-- position helpers
positions::[Int]
positions = [1..9]

stdblocks,nrcblocks::[[Int]]
stdblocks =[[1..3],[4..6],[7..9]]
nrcblocks = [[2..4],[6..8]]

alllocs::[Location]
alllocs = [(r,c) | r <- positions, c <- positions]

rowlocs, collocs,subgridlocs,allsubgridlocs::[[Location]]
rowlocs = [[(r,c) | c <- positions] | r <- positions]
collocs = [[(r,c) | r <- positions] | c <- positions]
subgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- stdblocks, cs <- stdblocks ]
nrcgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- nrcblocks, cs <- nrcblocks ]
allsubgridlocs = subgridlocs -- ++ nrcgridlocs

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


-- debugging helpers
solveAndShow::Grid -> IO[()]
solveAndShow grid = solveShowSnapShots $ initSnapShot grid
  where 
    solveShowSnapShots sss = (sequence . fmap showSnapShot . solveSnapShots) sss

genRandomSudoku::IO SnapShot
genRandomSudoku = do 
  [randomSnapShot] <- randomSolveSnapshot [emptySnapShot]
  return randomSnapShot

randomS::IO()
randomS = genRandomSudoku >>= showSnapShot

initSnapShot::Grid -> [SnapShot]
initSnapShot grid = 
  if (not . consistentGrid) valFinder then [] else [(valFinder, getconstraints valFinder)]
    where 
      valFinder = (\(r,c) -> (grid !! (r-1)) !! (c-1)) 
      consistentGrid valFinder = and $ (consisentRows ++ consisentCols ++ consisentSubs)
      consisentRows = map injective $ valsFrom rowlocs
      consisentCols = map injective $ valsFrom collocs
      consisentSubs = map injective $ valsFrom allsubgridlocs
      injective xs = nub xs == xs
      valsFrom locss = map (filter (/= 0)) [[valFinder loc | loc <- locs] | locs <- locss]

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

exampleNrc1::Grid
exampleNrc1 =   [[2,7,5,0,8,3,4,9,6],
            [8,4,1,5,9,6,3,7,2],
            [6,3,9,2,7,4,8,1,5],
            [4,8,6,7,1,9,5,2,3],
            [5,0,3,6,4,2,1,8,7],
            [7,1,2,3,5,8,6,0,9],
            [1,5,8,9,6,7,2,3,4],
            [3,6,7,4,2,1,9,5,8],
            [9,2,4,8,3,5,7,6,1]]

exampleRec1::Grid -- (2,5) matching rectangles cannot be unique
exampleRec1 = [[2,7,5,1,8,3,4,9,6],
                [8,4,1,0,9,6,3,7,0],
                [6,3,9,0,7,4,8,1,0],
                [4,8,6,7,1,9,5,2,3],
                [5,9,3,6,4,2,1,8,7],
                [7,1,2,3,5,8,6,4,9],
                [1,5,8,9,6,7,2,3,4],
                [3,6,7,4,2,1,9,5,8],
                [9,2,4,8,3,5,7,6,1]]
--
exampleRec2::Grid
exampleRec2 = [[2,7,5,1,8,3,4,9,6],
                [8,4,1,5,9,6,3,7,2],
                [6,3,9,2,7,4,8,1,5],
                [4,8,6,0,1,0,5,2,3],
                [5,9,3,6,4,2,1,8,7],
                [7,1,2,3,5,8,6,4,9],
                [1,5,8,0,6,0,2,3,4],
                [3,6,7,4,2,1,9,5,8],
                [9,2,4,8,3,5,7,6,1]]


-- solving strategies from http://www.sudokuwiki.org
nakedSingle = undefined
nakedDouble = undefined
nakedTripple = undefined
nakedsubSet = undefined
hiddenSingle = undefined
hiddenDouble = undefined
hiddenTripple = undefined
hiddenQuads = undefined
intersectionRemovalPointingPairs = undefined
intersectionRemovalPointingTripples = undefined
xwing = undefined 
singlesChain = undefined
ywing = undefined
swordfish = undefined
xyzwing = undefined
remotePairs = undefined
biValueUniversalGrave =undefined
xcycled = undefined
medusa = undefined
jellyfish = undefined
wxyzwing = undefined
xychains = undefined
alignedPairExclusion = undefined