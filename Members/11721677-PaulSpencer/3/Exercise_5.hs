module Exercise_5 where
import Lecture3
import Exercise_4
import Test.QuickCheck

{-taken from Ex3-}
fvs :: Form -> [Valuation]
fvs formula = [ row | row <- allVals formula, not (evl row formula) ]

valToProp :: (Name, Bool) -> Form
valToProp (name, bool) 
    | bool == False = Prop name
    | otherwise     = Neg (Prop name)

cnf :: Form -> Form
cnf = Cnj . map (Dsj . map valToProp) . fvs

{-end of Ex3-}

{-taken from Exercise 1-}
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

equiv :: Form -> Form -> Bool
equiv a b = tautology (Equiv a b )
{--}

type Clause  = [Int]
type Clauses = [Clause]
  
cnf2cls::Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg (Prop x)) = [[(x*(-1))]]
cnf2cls (Dsj fs) = (concat $ concat $ map cnf2cls fs):[]
cnf2cls (Cnj fs) = (concat $ map cnf2cls fs)

toProp n 
    | n < 0 = Neg (Prop (abs n))
    | otherwise = Prop n

cls2forms cs = Cnj (map (\fs -> Dsj ((map toProp) fs)) cs)

{-simple tests used whilst building-}
f0 = Prop 1
f0' = (Cnj [(Dsj [(Prop 1)])])
c0 = [[1]]

f1 = Neg (Prop 1)
f1' = (Cnj [(Dsj [(Neg (Prop 1))])])
c1 = [[-1]]

f2 = Dsj [(Prop 1), (Prop 2)]
f2' = (Cnj [   (Dsj [ (Prop 1),(Prop 2) ])  ])
c2 = [[1,2]]

f3 = Cnj [(Prop 1), (Prop 2)]
f3' = (Cnj [(Dsj [(Prop 1)]) , (Dsj [(Prop 2)])])
c3 = [[1],[2]]

f4 = Cnj [(Prop 1), (Dsj [(Prop 2), (Neg (Prop 3))])]
f4' = Cnj [(Dsj [(Prop 1)]), (Dsj [(Prop 2), (Neg (Prop 3))])]
c4 = [[1],[2,-3]] 

testF2C = map cnf2cls [f0,f1,f2,f3,f4] == [c0,c1,c2,c3,c4] 
testC2F = map cls2forms [c0,c1,c2,c3] == [f0',f1',f2',f3'] 

--  could try to build an SAT evaluator, but instead we will just return it to Form


{-Tests-}
prop_equivalent form = (cnf form) `equiv` (cls2forms $ cnf2cls $ cnf form)
--  +(+(14 9 1 4092 11 3 3 5) (4<=>-1))
{-end Tests-}