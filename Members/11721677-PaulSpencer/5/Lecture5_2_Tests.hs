module Lecture5_2_Tests where
import Lecture5_2
import Test.QuickCheck
import Data.List
import Debug.Trace
import Control.Monad

import System.IO.Unsafe



-- generators
rowGen::Gen Row
rowGen = choose(1,9)

colGen:: Gen Column
colGen = choose(1,9)

valGen::Gen Value
valGen = choose(1,9)

locGen:: Gen (Row,Column)
locGen = (,) <$> rowGen <*> colGen

solvedCellGen::Gen ((Row, Column),Value)
solvedCellGen = (,) <$> locGen <*> valGen

valsGen::Gen [Value]
valsGen = do
    valscount <- choose (0,9)
    permpick <- choose (1, length $ permutations [1..9])
    return (sort $ take valscount $ permutations [1..9] !! (permpick-1))

conGen::Gen Constraint
conGen = do
    loc <- locGen
    vals <-valsGen
    return (fst loc, snd loc, vals)

conGen'::(Row,Column) -> Gen Constraint
conGen' loc = do
    vals <-valsGen
    return (fst loc, snd loc, vals)

alllocs::[(Row,Column)]
alllocs = [(r,c) | r <- positions, c <- positions]

consGen::Gen [Constraint]
consGen = do
    sequence [ conGen' loc | loc <- alllocs ]

sudokuGen::Gen Sudoku
sudokuGen = do
    loc <- locGen
    val <- valGen
    sud <- sudokuGen
    return ( (\l -> if loc == l then val else sud l))

-- to do - make generative
gridGen::Gen Grid
gridGen = return example1

-- type SnapShot = (ValueAtLocation,[Constraint])
snapShotGen::Gen Node
snapShotGen = (,) <$> sudokuGen <*> consGen


showTestGrid :: Grid -> String
showTestGrid [as,bs,cs,ds,es,fs,gs,hs,is] = 
    (showB as bs cs) ++ 
    (showB ds es fs) ++ 
    (showB gs hs is) ++ 
    "\n" ++ 
    hline
    where
        showB aas bbs ccs = (hline ++ (showR aas) ++ (showR bbs) ++ (showR ccs))
        showR [a1,a2,a3,a4,a5,a6,a7,a8,a9] =  ((showC a1 a2 a3) ++ (showC a4 a5 a6) ++ (showC a7 a8 a9) ++ "|\n")
        showC aa1 aa2 aa3 = ("| " ++ (showVal aa1) ++ " " ++ (showVal aa2) ++ " " ++ (showVal aa3) ++ " ")
        hline = "+-------+-------+-------+\n"

---

-- tests
-- srg::Gen (Sudoku, Row)
-- srg = ((,) <$> sudokuGen <*> rowGen)
-- p::(Sudoku, Row) -> Bool
-- p =  (\(sud, r) -> length (freeInRow sud r) > 9)
newtype MySudoku = 
-- prop_freeInRow::Row -> Property
prop_freeInRow sud = forAll rowGen $ (\r -> length (freeInRow sud r) > 9)
-- prop_freeInRow g = forAll srg 
--test_freeInRow = quickCheck prop_freeInRow


-- test_freeInRow1 = quickCheck prop_freeInRow
-- prop_correctNumberOfDerangements::Integer -> Bool
-- prop_correctNumberOfDerangements n = (derangementSize n) == (iLength $ deran n)
-- prop_correctNr_limited = forAll performanceLimit 


-- freeInRow :: Sudoku -> Row -> [Value]
-- freeInRow s r = 
--   freeInSeq [ s (r,i) | i <- positions  ]
-- freeInSeq :: [Value] -> [Value]
-- freeInSeq seq = values \\ seq 
