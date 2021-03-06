module Exercise_2 where 

import Data.List
import System.Random

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

-- logisticians Update

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

type Position = (Row,Column)
type Constrnt = [[Position]]

rowconstrnt = [[(r,c)| c <- values ] | r <- values ]
colcnstraint = [[(r,c)| r <- values ] | c <- values ]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

blockconstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let
    ys = filter (elem (r,c)) xs
    in
    foldl1 intersect (map ((values \\) . map s) ys)

constrnts :: Constrnt
constrnts = rowconstrnt ++ colcnstraint ++ blockconstrnt

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
    showGrid' vs (pos@(r,c) : ps)
        | c == (last cs)     = colEnd
        | pos `elem` vlocs   = value'
        | pos `elem` corners = corner
        | pos `elem` vlines  = vline
        | pos `elem` hlines  = hline
        | otherwise          = empty
        where
            value' = next valueAtLocation
            valueAtLocation = maybe ' ' head  $ lookup pos $ zip vlocs (map (showVal) (concat vs)) 
            colEnd = next '\n'
            corner = next '+'
            vline = next '|'
            hline = next '-'
            empty = next ' '
            next c = do putChar c ; showGrid' vs ps

-- postion helpers

alllocs::[Position]
alllocs = [(r,c) | r <- positions, c <- positions]

rowlocs, collocs,subgridlocs::[[Position]]
rowlocs = [[(r,c) | c <- positions] | r <- positions]
collocs = [[(r,c) | r <- positions] | c <- positions]
subgridlocs = [[(r, c) | r <- rs, c <- cs] | rs <- blocks, cs <- blocks ]

--

type Sudoku = Position -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = [[s (r,c) | c <- [1..9] ] | r <- [1..9]] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
    where 
    pos :: [[a]] -> (Row,Column) -> a 
    pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

subGrid :: Sudoku -> Constrnt -> Position -> [Value]
subGrid s cs pos = [s v | v <- concat $ filter (elem pos)  cs]

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective,colInjective,subgridInjective :: Sudoku -> Position -> Bool
rowInjective s (r,_) = injective $ gone $ [s pos | pos <- rowlocs !! (r-1)]
colInjective s (_,c) = injective $ gone $ [s pos | pos <- collocs !! (c-1)]
subgridInjective s pos = injective $ gone $ (subGrid s blockconstrnt pos)

gone::[Value] -> [Value]
gone vals = filter (/= 0) vals 

consistent :: Sudoku -> Bool
consistent s = and $ consistentRow ++ consistentCol ++ consistentSub
    where 
    consistentRow = isInjective rowInjective rowlocs
    consistentCol = isInjective colInjective collocs
    consistentSub = isInjective subgridInjective subgridlocs
    isInjective inj locs =  [inj s pos | pos <- map head locs ]

extend :: Sudoku -> (Position,Value) -> Sudoku
extend s (pos,v) pos' 
    | pos == pos' = v
    | otherwise   = s pos'

type Node = (Sudoku,[Position])

showNode :: Node -> IO()
showNode (s,_) = showSudoku s

solved  :: Node -> Bool
solved (_,c) = null c

extendNode :: Node -> Position -> [Node]
extendNode (s, cs) pos = [(extend s (pos,v), cs) | v <- freeAtPos' s pos constrnts]

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
                if (not . consistent) s then [] 
                else [(s, constraints s)]

openPositions :: Sudoku -> [Position]
openPositions s = [ pos | pos <- alllocs, s pos == 0 ]
                
valLen :: (Position,[Value]) -> (Position,[Value]) -> Ordering
valLen (_,vs) (_,vs') = compare (length vs) (length vs')

constraints :: Sudoku -> [Position]
constraints s = [pos | (pos, _) <- (sortBy valLen freePositions)]
    where
    freePositions = [(pos, freeAtPos' s pos constrnts) | pos <- openPositions s ]

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
solveShowNs = sequence . fmap showNode . solveNs

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
getRandomItem xs = do 
    n <- getRandomInt maxi
    return [xs !! n]
        where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do 
    y <- getRandomItem xs 
    if null y 
        then return []
        else do 
            ys <- randomize (xs\\y)
            return (head y:ys)

type Constraint = (Row,Column,[Value])

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs) 
    where 
        f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = do 
    xs <- getRandomCnstr cs
    if null xs 
        then return []
        else return (extendNode (s,cs\\xs) (head xs))

rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (Node -> IO [Node]) -> (Node -> Bool) -> IO [Node] -> IO [Node]
rsearch succ goal ionodes = do 
    xs <- ionodes 
    if null xs 
        then return []
        else 
            if goal (head xs) 
            then return [head xs]
            else do 
                ys <- rsearch succ goal (succ (head xs))
                if (not . null) ys 
                    then return [head ys]
                    else if null (tail xs) then return []
                        else 
                            rsearch 
                            succ goal (return $ tail xs)


genRandomSudoku :: IO Node
genRandomSudoku = do 
    [r] <- rsolveNs [emptyN]
    return r

randomS = genRandomSudoku >>= showNode

uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) where 
    singleton [] = False
    singleton [x] = True
    singleton (x:y:zs) = False

eraseS :: Sudoku -> (Row,Column) -> Sudoku
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                        | otherwise      = s (x,y)

eraseN :: Node -> (Row,Column) -> Node
eraseN n (r,c) = (s, constraints s) 
    where s = eraseS (fst n) (r,c) 

minimalize :: Node -> [(Row,Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) 
    | uniqueSol n' = minimalize n' rcs
    | otherwise    = minimalize n  rcs
        where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = [ (r,c) | r <- positions,  
                                c <- positions, s (r,c) /= 0 ]

genProblem :: Node -> IO Node
genProblem n = do 
    ys <- randomize xs
    return (minimalize n ys)
        where xs = filledPositions (fst n)

main :: IO ()
main = do 
    [r] <- rsolveNs [emptyN]
    showNode r
    s  <- genProblem r
    showNode s