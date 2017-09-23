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

-- test of generator
-- 1 formula parses: generator provides valid formulas

prop_validForm form = (head $ parse $ show form) == form
test_validForm = verboseCheck prop_validForm

-- inspired by http://www.programming-idioms.org/idiom/82/count-substring-occurrences/999/haskell
opCount s = sum [ 1 | r <- tails s, ss <- ["-","==>","<=>","*(","+("], isPrefixOf ss r ]


-- toine -> we are randomly generating Forms, thus the Forms are no longer pure, thus we can only deal with
-- them within the monads they are associated (in our case we are using the QuickCheck Gen Monad, other options
-- are for example the IO monad)  you can interact with them within the context of the Gen Monad () or by using
-- quickcheck properties you can look at then using the generate function:
--   generate (genForm 10)


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