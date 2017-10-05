module Lecture5_Tests where
import Lecture5
import Test.QuickCheck
import Data.List
import Control.Monad

rowGen::Gen Row
rowGen = choose(1,9)

colGen:: Gen Column
colGen = choose(1,9)

valGen::Gen Value
valGen = choose(1,9)

locGen:: Gen Location
locGen = (,) <$> rowGen <*> colGen

solvedCellGen::Gen SolvedCell
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
    return ((loc, vals))

conGen'::Location -> Gen Constraint
conGen' loc = do
    vals <-valsGen
    return ((loc, vals))

consGen::Gen [Constraint]
consGen = do 
    sequence [ conGen' loc | loc <- alllocs ]


vAtLocGen::Gen ValueAtLocation
vAtLocGen = do
    loc <- locGen
    val <- valGen
    vAtLoc <- vAtLocGen
    return ( (\l -> if loc == l then val else vAtLoc  l))

-- to do - make generative
gridGen::Gen Grid
gridGen = return exampleNrc1

-- type SnapShot = (ValueAtLocation,[Constraint])
snapShotGen::Gen SnapShot
snapShotGen = (,) <$> vAtLocGen <*> consGen


-- TestNakedSingle
-- precondition = has unique naked single, and same number elsewhere

-- postcondition = removed all but unique naked single