module Lab3 where
import Data.List
import Lecture3
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

contradiction :: Form -> Bool
contradiction f = not $ satisfiable f

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails a b = tautology (Impl a b)

equiv :: Form -> Form -> Bool
equiv a b = tautology (Equiv a b )

-- Q3

cnf :: Form -> Form
cnf = cnfGen . nnf . arrowfree 

cnfGen :: Form -> Form
cnfGen (Prop x)         = Prop x
cnfGen (Neg (Prop x))   = Neg (Prop x)
cnfGen (Cnj xs)         = Cnj (map cnfGen xs) 

cnfGen (Dsj [])         = Dsj []
cnfGen (Dsj [x])        = cnfGen x
cnfGen (Dsj (x:xs))     = dsj2cnj (cnfGen x) (cnfGen (Dsj xs))

dsj2cnj :: Form -> Form -> Form
dsj2cnj (Cnj []) _      = Cnj []
dsj2cnj _ (Cnj [])      = Cnj []
dsj2cnj x f             = Dsj [x,f]

dsj2cnj (Cnj [x]) f     = dsj2cnj x f
dsj2cnj f (Cnj [x])     = dsj2cnj (Cnj [x]) f

dsj2cnj (Cnj (x:xs)) f  = Cnj [dsj2cnj x f, dsj2cnj (Cnj xs) f]
dsj2cnj f (Cnj (x:xs))  = dsj2cnj (Cnj (x:xs)) f


a = Prop 1
b = Prop 2
c = Prop 3
d = Prop 4
e = Prop 5
g = Prop 6

testInput :: [Form]
testInput = [
  Dsj[a],
  Cnj[b],
  Dsj[a,b],
  Cnj[a,b,c,d,e,g],
  Impl a b,
  Impl b a,
  Equiv c d,
  Dsj[Impl a b, Impl c d],
  Impl (Cnj [a]) (Dsj[b,c,d])
  ]

cnfTest :: Form -> Bool
cnfTest x
    | tautology (x) = True
    | otherwise = equiv x (cnf x)

run = map cnfTest testInput