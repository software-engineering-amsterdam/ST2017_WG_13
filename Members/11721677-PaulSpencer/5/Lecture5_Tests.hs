module Lecture5_Tests where
import Lecture5
import Test.QuickCheck

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
    return (take valscount [1..])

conGen::Gen Constraint
conGen = do 
    loc <- locGen
    vals <-valsGen
    return ((loc, vals))
-- to do make better (not grid)
vAtLocGen::Gen ValueAtLocation
vAtLocGen = do
    loc <- locGen
    val <- valGen
    vAtLoc <- vAtLocGen
    return ( (\l -> if loc == l then val else vAtLoc  l))

-- to do - make more generative
gridGen::Gen Grid
gridGen = return exampleNrc1

-- to do gridGen type Grid        = [[Value]]

-- type SnapShot = (ValueAtLocation,[Constraint])