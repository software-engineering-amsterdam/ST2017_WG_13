module Lab3 where
import Data.List
import Lecture3
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Q3
{-
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)
-}

cnfGenerator :: Form -> Form
cnfGenerator = cnf . nnf . arrowfree 


cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)

cnf (Dsj [f, Cnj [p,q]]) = Cnj [ Dsj [cnf f, cnf p], Dsj [cnf f, cnf q] ]

cnf (Dsj [Cnj [p,q], f]) = cnf (Dsj [f, Cnj [p,q]])

cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj fs) = Dsj (map cnf fs)