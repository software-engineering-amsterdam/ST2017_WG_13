module Exercise_4 where
import Data.List
import Test.QuickCheck
import Control.Monad
import Lecture3

{-
  commentry on this question is found on the team wiki : 
-}

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


propositionGeneratorWithIdBetween::Int -> Int -> LogicFormulaGenerator
propositionGeneratorWithIdBetween min max = liftM Prop (choose (min, max))


binaryLogicFormulaGenerator:: BinaryLogicOperation -> RemainingOperatorCount -> LogicFormulaGenerator 
binaryLogicFormulaGenerator binaryLogicOperation max = 
    liftM2 binaryLogicOperation 
        (arbitraryFormulaGenerator $ updatedMax max 2) 
        (arbitraryFormulaGenerator $ updatedMax max 2)

updatedMax::RemainingOperatorCount -> BranchCount -> RemainingOperatorCount
updatedMax maxRemainingFormulas branchCount = maxRemainingFormulas `div` branchCount


listOfLogicOperatorsGenerator::MultipleLogicOperation -> RemainingOperatorCount -> LogicFormulaGenerator
listOfLogicOperatorsGenerator multipleLogicOperation max = frequency [
    (80, liftM multipleLogicOperation (listOfLogicOperatorsGeneratorBetween max 2 3)),
    (19, liftM multipleLogicOperation (listOfLogicOperatorsGeneratorBetween max 4 10)),
    (1, liftM multipleLogicOperation (listOfLogicOperatorsGeneratorBetween max 11 50))]

listOfLogicOperatorsGeneratorBetween::RemainingOperatorCount -> Int -> Int -> MultipleLogicFormulaGenerator
listOfLogicOperatorsGeneratorBetween maxRemaining minLength maxLength = oneof $ 
    [listOfLogicOperators maxRemaining actualLength | actualLength <- [minLength..maxLength] ]

listOfLogicOperators::RemainingOperatorCount -> BranchCount -> MultipleLogicFormulaGenerator
listOfLogicOperators max len = replicateM len (arbitraryFormulaGenerator (updatedMax max len))


type LogicFormula = Form
type LogicFormulaGenerator = Gen Form
type MultipleLogicFormulaGenerator = Gen [Form]
type RemainingOperatorCount = Int
type BinaryLogicOperation = (Form -> Form -> Form)
type MultipleLogicOperation =  [Form] -> Form
type BranchCount = Int

-- 1) get a random number (gen 1) (gen 2) (gen 2 -> 50)
-- 2) distribute that number across a list
-- 3) Gen [Form]
-- 4) Gen Fom 

-- data Form = Prop Name
-- | Neg  Form
-- | Cnj [Form]
-- | Dsj [Form]
-- | Impl Form Form 
-- | Equiv Form Form 
-- deriving (Eq,Ord)
