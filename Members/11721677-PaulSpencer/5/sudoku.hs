module Sudoku where
import Data.List as List
import Data.Maybe
import Data.Set as Set 

data Board = Board [Cell]
data Row = Row Id [Cell]
data Column = Column Id [Cell] 
data Square = Square Id [Cell]
type Cell = (Location, FinalChoice, RemainingChoices)

type Id = Int
type Location = (Id, Id, Id)
type FinalChoice = Maybe Value
type RemainingChoices = Set.Set Value
data Value = One | Two | Three | Four | Five | Six | Seven | Eight | Nine

boardFromList :: [[Int]] -> Board
boardFromList cs = undefined

input::[[Int]]
input =  [[11..19],[21..29],[31..39],[41..49],[51..59],[61..69],[71..79],[81..89],[91..99]]

rowsToColumns::[[Int]] -> [[Int]]
rowsToColumns []             = []
rowsToColumns ([]   : xss)   = rowsToColumns xss
rowsToColumns ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : rowsToColumns (xs : [ t | (_:t) <- xss])

-- rowsToSquares::[[Int]] -> [[Int]]
-- rowsToSquares []                   = []
-- rowsToSquares ([]   : xss)         = rowsToSquares xss
-- --rowsToSquares ((x:y:z:xs) : xss) = (x:y:z : ([h:i:j | (h:i:j:_) <- xss])) : rowsToSquares (xs : [ t | (_:_:_:t) <- xss])
-- --rowsToSquares ((x:y:z:xs) : (s:t:sss)) = (x:y:z : [s' | s' <- concat [s,t,u]]) : rowsToSquares (xs : [ t' | (_:_:_:t') <- sss])
-- --rowsToSquares ((x:y:z:xs) : (ss:ts:sss)) = (x:y:z : [s | s <- rowsToSquares [ss,ts]])
-- --rowsToSquares (xs:xss) = (pure $ concat $ (take 3 xs) :  rowsToSquares (take 2 xss)) ++ (rowsToSquares $ drop 2 xss)
-- rowsToSquares (xs:xss) = 
--     (pure $ concat $ (take 3 xs) :  rowsToSquares (take 2 xss)) 
--     ++ ((drop 3 xs) : (rowsToSquares (drop 2 xss)))

rowsToSquares::[[Int]] -> [[Int]]
rowsToSquares []                   = []
rowsToSquares ([]   : xss)         = rowsToSquares xss
rowsToSquares ((xs:ys:zs) : xss)   = rowsToSquares xss




blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]


-- rowBandsFromLists:: [[Int]] -> (RowBand, RowBand, RowBand)
-- rowBandsFromLists = undefined

-- columnBandsFromLists:: [[Int]] -> (ColumnBand, ColumnBand, ColumnBand)
-- columnBandsFromLists = undefined

