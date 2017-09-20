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
    arbitrary = sized arbitraryFormulaGenerator

arbitraryFormulaGenerator ::RemainingOperatorCount -> LogicFormulaGenerator
arbitraryFormulaGenerator 0 = propositionGenerator
arbitraryFormulaGenerator max = oneof [
    (negationGenerator max),
    (implicationGenerator max),
    (equivilanceGenerator max),
    (disjunctionGenerator max),
    (conjunctionGenerator max),
    (propositionGenerator)]

propositionGenerator:: LogicFormulaGenerator
propositionGenerator = frequency [
    (50, propositionGeneratorWithIdBetween 1 3),
    (30, propositionGeneratorWithIdBetween 4 7),
    (15, propositionGeneratorWithIdBetween 8 20),
    (5, propositionGeneratorWithIdBetween 21 10000)]

propositionGeneratorWithIdBetween::Int -> Int -> LogicFormulaGenerator
propositionGeneratorWithIdBetween min max = liftM Prop (choose (min, max))

negationGenerator :: RemainingOperatorCount -> LogicFormulaGenerator
negationGenerator max = liftM Neg (arbitraryFormulaGenerator (max - 1))

implicationGenerator :: RemainingOperatorCount -> LogicFormulaGenerator
implicationGenerator max = binaryLogicFormulaGenerator Impl max

equivilanceGenerator :: RemainingOperatorCount -> LogicFormulaGenerator
equivilanceGenerator max = binaryLogicFormulaGenerator Equiv max

disjunctionGenerator :: RemainingOperatorCount -> LogicFormulaGenerator
disjunctionGenerator max = listOfLogicOperatorsGenerator Dsj max

conjunctionGenerator :: RemainingOperatorCount -> LogicFormulaGenerator
conjunctionGenerator max = listOfLogicOperatorsGenerator Cnj max

binaryLogicFormulaGenerator:: BinaryLogicOperation -> RemainingOperatorCount -> LogicFormulaGenerator 
binaryLogicFormulaGenerator binaryLogicOperation max = 
    liftM2 binaryLogicOperation 
        (arbitraryFormulaGenerator $ updatedMax max 2) 
        (arbitraryFormulaGenerator $ updatedMax max 2)

updatedMax::RemainingOperatorCount -> Int -> RemainingOperatorCount
updatedMax max splitSize = max `div` splitSize

listOfLogicOperatorsGenerator::MultipleLogicOperation -> RemainingOperatorCount -> LogicFormulaGenerator
listOfLogicOperatorsGenerator multipleLogicOperation max = frequency [
    (80, liftM multipleLogicOperation (listOfLogicOperatorsGeneratorBetween max 2 3)),
    (19, liftM multipleLogicOperation (listOfLogicOperatorsGeneratorBetween max 4 10)),
    (1, liftM multipleLogicOperation (listOfLogicOperatorsGeneratorBetween max 11 50))]

listOfLogicOperatorsGeneratorBetween::RemainingOperatorCount -> Int -> Int -> MultipleLogicFormulaGenerator
listOfLogicOperatorsGeneratorBetween maxRemaining minLength maxLength = oneof $ 
    [listOfLogicOperators maxRemaining actualLength | actualLength <- [minLength..maxLength] ]

listOfLogicOperators::RemainingOperatorCount -> Int -> MultipleLogicFormulaGenerator
listOfLogicOperators max len = replicateM len (arbitraryFormulaGenerator (updatedMax max len))

type LogicFormula = Form
type LogicFormulaGenerator = Gen Form
type MultipleLogicFormulaGenerator = Gen [Form]
type RemainingOperatorCount = Int
type BinaryLogicOperation = (Form -> Form -> Form)
type MultipleLogicOperation =  [Form] -> Form