module Exercise_2 where 
import Data.List
import System.Random


type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

--- Display Grid
showVal::Value -> String
showVal 0 = " "
showVal d = show d

type DrawLoc = (Int, Int)
cs = [0..31]
rs = [0..18]

vlocs = [(r,c) | r <- rs, c <- [2,5,8,12,15,18,22,25,28], odd r]
mainGridHLines = [0,6,12,18]
mainGridVLines = [0,10,20,30]


corners::[DrawLoc]
corners  = [(r,c) | r <- mainGridHLines, c <- mainGridVLines]

vlines::[DrawLoc]
vlines = [(r,c) | r <- rs, c <- mainGridVLines]

hlines::[DrawLoc]
hlines = [(r,c) | r <- mainGridHLines, c <- cs]

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

-- position helpers
positions::[Int]
positions = [1..9]

stdblocks::[[Int]]
stdblocks =[[1..3],[4..6],[7..9]]

alllocs::[Position]
alllocs = [(r,c) | r <- positions, c <- positions]

rowlocs, collocs,subgridlocs::[[Position]]
rowlocs = [[(r,c) | c <- positions] | r <- positions]
collocs = [[(r,c) | r <- positions] | c <- positions]
subgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- stdblocks, cs <- stdblocks ]


-- logisticians update
values = [1..9]

type Position = (Row,Column)
type Constrnt = [[Position]]

allConstraints, blockConstraint::[[Position]]
blockConstraint = [[(r,c) | r <- b1, c <- b2 ] | b1 <- stdblocks, b2 <- stdblocks]
allConstraints = rowConstraint ++ columnConstraint ++ blockConstraint
    where
        rowConstraint = [[(r,c) | c <- positions ] | r <- positions ]
        columnConstraint = [[(r,c) | r <- positions ] | c <- positions ]

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s pos xs = 
    let 
        ys = filter (elem pos) xs
    in
        foldl1 intersect (map ((values \\) . map s) ys)

--

type Sudoku = Position -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = map (map s) rowlocs 

grid2sud :: Grid -> Sudoku
grid2sud gr = \loc -> pos gr loc 
    where 
    pos :: Grid -> Position -> Value 
    pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

-- sglocs4loc::Position -> [[Position]] -> [Position]
-- sglocs4loc loc slocs = concat $ filter (elem loc) slocs


-- subGrid :: Sudoku -> Constrnt -> Position -> [Value]
-- subGrid s cs loc = [s loc' | loc' <- sglocs4loc loc cs]

oneOfR :: Int->[Position]->Bool
oneOfR r  constraint = elem r [fst d|d<-constraint] 

oneOfC :: Int->[Position]->Bool
oneOfC c constraint = elem c [snd d|d<-constraint]

subGrid :: Sudoku -> Constrnt->(Row,Column) -> [Value]
subGrid s constraints (r,c) = 
    [ s d | d<- concat $ filter (oneOfC c) (filter (oneOfR r)  constraints) ]


-- freeInSeq :: [Value] -> [Value]
-- freeInSeq seq = values \\ seq 

-- rowValues,freeInRow::Sudoku -> Row -> [Value]
rowValues s r = [s loc | loc <- rowlocs !! (r-1)]
-- freeInRow s r = freeInSeq $ rowValues s r

-- columnValues,freeInColumn::Sudoku -> Column -> [Value]
columnValues s c = [s loc | loc <- collocs !! (c-1)]
-- freeInColumn s c = freeInSeq $ columnValues s c

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective,colInjective :: Sudoku -> Position -> Bool
rowInjective s (r,_) = injective $ gone $ rowValues s r
colInjective s (_,c) = injective $ gone $ columnValues s c

--subgridInjective s loc = injective $ gone $ (subGrid s blockConstraint loc)
subgridInjective s (r,c) = injective vs where 
    vs = filter (/= 0) (subGrid s blockConstraint (r,c))

gone::[Value] -> [Value]
gone vals = filter (/= 0) vals 

consistent :: Sudoku -> Bool
consistent s = and $ consistentRow ++ consistentCol ++ consistentSub
    where 
    consistentRow = isInjective rowInjective rowlocs
    consistentCol = isInjective colInjective collocs
    consistentSub = isInjective subgridInjective subgridlocs
    isInjective inj locs =  [inj s loc | loc <- map head locs ]

extend :: Sudoku -> (Position,Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x 


type Node = (Sudoku,[Position])

showNode :: Node -> IO()
showNode (s,_) = showSudoku s

solved  :: Node -> Bool
solved (_,c) = null c

-- extendNode :: Node -> Position -> [Node]
-- extendNode (s,cs) loc = [(extend s (loc,v), cs) | v <- freeAtPos' s loc allConstraints]

extendNode :: Node -> Position -> [Node]
extendNode (s,constraints) (r,c) = 
   [(extend s ((r,c),v), constraints) | v <- freeAtPos' s (r, c) allConstraints]


valLen :: (Position,[Value]) -> (Position,[Value]) -> Ordering
valLen (_,vs) (_,vs') = compare (length vs) (length vs')

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]

openPositions :: Sudoku -> [Position]
openPositions s = [ loc | loc <- alllocs, s loc == 0 ]

-- constraints :: Sudoku -> [Position] 
-- constraints s = sortBy valLen [(loc, freeAtPos' s loc) | loc <- openPositions s ]


constraints :: Sudoku -> [Position]
constraints s = [(r,c)|(r, c, _)<-(sortBy length3rd 
    [(r,c, freeAtPos' s (r,c) allConstraints) | 
                       (r,c) <- openPositions s ])]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')
                       
-- constraints :: Sudoku -> [Position]
-- constraints s = 
--     [loc | (loc, _) <- (sortBy valLen [(loc, freeAtPos' s loc allConstraints) | loc <- openPositions s ])]

search :: (Node -> [Node]) -> (Node -> Bool) -> [Node] -> [Node]
search getChild goal [] = []
search getChild goal (x:xs) 
    | goal x    = x : search getChild goal xs
    | otherwise = search getChild goal ((getChild x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search childNode solved 

childNode :: Node -> [Node]
childNode (s,[]) = []
childNode (s,p:ps) = extendNode (s,ps) p 

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs ns = sequence $ fmap showNode (solveNs ns)

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



example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
           [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8], 
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]

emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs 
                  if null y 
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

rsearch :: (node -> IO [node]) 
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = 
  do xs <- ionodes 
     if null xs 
       then return []
       else 
         if goal (head xs) 
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys 
                      then return [head ys]
                      else if null (tail xs) then return []
                           else 
                             rsearch 
                               succ goal (return $ tail xs)


uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseS :: Sudoku -> Position -> Sudoku
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)

eraseN :: Node -> Position -> Node
eraseN n (r,c) = (s, constraints s) 
  where s = eraseS (fst n) (r,c) 

minimalize :: Node -> [(Row,Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                         | otherwise    = minimalize n  rcs
  where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = [ (r,c) | r <- positions,  
                              c <- positions, s (r,c) /= 0 ]

-- genProblem :: Node -> IO Node
-- genProblem n = do ys <- randomize xs
