module Lab3 where
import Data.List
import Test.QuickCheck
import Control.Monad
import Lecture3

-- Deliverables: 
-- 🗹 generator for formulas
-- ☒ sequence of test properties
-- ☒ test report
-- ☒ indication of time spent

instance Arbitrary Form where
    arbitrary = sized arbForm

-- adapted from Quick Check A Lightweight Tool for Random Testing of Haskell Programs - 
-- Koen Claessen, John Hughes
-- https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/slides/meiser.pdf
arbForm ::Int -> Gen Form
arbForm 0 = propGen
arbForm depth = frequency [
    (2, propGen),
    (6, negGen depth),
    (6, implGen depth),
    (6, equivGen depth),
    (6, liftM Cnj (formListGenSmall depth)),
    (6, liftM Dsj (formListGenSmall depth)),
    (1, liftM Cnj (formListGenLarge depth)),
    (1, liftM Dsj (formListGenLarge depth))]

propGen:: Gen Form
propGen = frequency [
    (10, liftM Prop (choose (1,3))),
    (7, liftM Prop (choose (4,7))),
    (3, liftM Prop (choose (8,20))),
    (1, liftM Prop (choose (20,10000)))]

negGen :: Int -> Gen Form
negGen depth = liftM Neg (arbForm (depth-1))

implGen :: Int -> Gen Form
implGen depth = liftM2 Impl (arbForm (depth `div` 2)) (arbForm (depth `div` 2))

equivGen :: Int -> Gen Form
equivGen depth = liftM2 Equiv (arbForm (depth `div` 2)) (arbForm (depth `div` 2))

cnjGen depth = frequency [
    (20,liftM Dsj (formListGenSmall depth)),
    (5,liftM Dsj (formListGenLarge depth)),
    (1,liftM Dsj (formListGenVeryLarge depth))]

-- inspired by carls answer in 
-- https://stackoverflow.com/questions/25300551/multiple-arbitrary-calls-returning-same-value
formListGenSmall::Int -> Gen [Form]
formListGenSmall depth = oneof $ [replicateM n (arbForm (depth `div` n)) | n <- [1..3] ]

formListGenLarge::Int -> Gen [Form]
formListGenLarge depth = oneof $ [replicateM n (arbForm (depth `div` n)) | n <- [4..10] ]

formListGenVeryLarge::Int -> Gen [Form]
formListGenVeryLarge depth = oneof $ [replicateM n (arbForm (depth `div` n)) | n <- [11..50] ]


-- Test Properties
-- 1) test if the truth tables of the input formula and the output CNF is the equivalent

-- *(+(-1 (7<=>4) *(6 20)))