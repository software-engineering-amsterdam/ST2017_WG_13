module Exercise_4 where
import Data.List
import Test.QuickCheck
import Lecture3
import Exercise_1
import Exercise_3

{-
    all notes for this are available at: 
    https://github.com/software-engineering-amsterdam/ST2017_WG_13/blob/master/Team/3/Exercise_4.md
-}

opCount ops s = sum [ 1 | r <- tails s, ss <-ops , isPrefixOf ss r ]

genProp_validOpCount = do
    s <- choose (0,10000)
    form <- formGen s
    return ((opCount ["-","==>","<=>","*(","+("] $ show form) == s) 
test_ValidOpCount = verboseCheck $ forAll genProp_validOpCount (==True)

prop_validForm form = (head $ parse $ show form) == form
test_validForm = verboseCheck prop_validForm

prop_validCnfConversion form = equiv form (formToCnf form)
test_validCnfConversion = verboseCheck prop_validCnfConversion

instance Arbitrary Form where
    arbitrary = sized formGen

formGen ::Int -> Gen Form
formGen s 
    | s <= 0    = propGen
    | s == 1    = negGen s
    | otherwise =  do
        arity <- arityGen
        frm <- (case arity of
                One -> negGen s
                Two -> imEqGen s
                Many -> cjDjGen s)
        return frm

propGen::Gen Form
propGen = do
    id <- propIdGen
    return (Prop id)

negGen:: Int -> Gen Form
negGen s = do
    fs <- formDistributionGen s 1
    return (Neg (head fs))

imEqGen:: Int -> Gen Form
imEqGen s = do
    fs <- formDistributionGen s 2
    ctor <- elements [Impl, Equiv]
    return (ctor (head fs) (last fs))

cjDjGen:: Int -> Gen Form
cjDjGen s = do
        len <- lenGen 
        fs <- formDistributionGen s len
        ctor <- elements [Dsj, Cnj]
        return (ctor fs)

formDistributionGen:: Int -> Int -> Gen [Form]
formDistributionGen s len = fdg (s-1) len
    where
        fdg s' 1 = do
            f <- formGen s'
            return [f]  
        fdg s' len' = do
            rn <- boundedDistributionGen s' len'
            fs <- fdg (s'- rn) (len'-1)
            f <- formGen rn
            return (f : fs)

boundedDistributionGen::Int -> Int -> Gen Int
boundedDistributionGen num len = do
    rn <- choose (0, num)
    rd <- choose (1, len) 
    return (rn `div` rd)

data Arity = One | Two | Many

arityGen::Gen Arity
arityGen = elements [One, Two, Two, Many, Many]

propIdGen::Gen Int
propIdGen = frequency [
    (60, choose(1, 3)),
    (30, choose(4, 7)),
    (9, choose(8, 20)),
    (1, choose(21, 10000000))]

lenGen::Gen Int
lenGen = frequency [
    (90, choose(2, 3)),
    (9, choose(4, 10)),
    (1, choose(11, 50))]