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
    arbitrary = sized createArbitraryLogicEquasion

type LogicFormula = Form
type LogicFormulaGenerator = Gen Form
type RemainingOperators = Int
type BinaryLogicOperation = (Form -> Form -> Form)
type MultipleLogicOperation =  [Form] -> Form


-- adapted from Quick Check A Lightweight Tool for Random Testing of Haskell Programs - 
-- Koen Claessen, John Hughes
-- https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/slides/meiser.pdf
createArbitraryLogicEquasion ::RemainingOperators -> LogicFormulaGenerator
createArbitraryLogicEquasion 0 = addProposition
createArbitraryLogicEquasion max = addArbitraryEquasion max
    
addArbitraryEquasion :: RemainingOperators -> LogicFormulaGenerator
addArbitraryEquasion max = oneof [
    (addNegation max),
    (addImplication max),
    (addEquivilance max),
    (addConjunction max),
    (addDisjunction max),
    (addProposition)]

addProposition:: LogicFormulaGenerator
addProposition = frequency [
    (50, createPropositionWithIdBetween 1 3),
    (30, createPropositionWithIdBetween 4 7),
    (15, createPropositionWithIdBetween 8 20),
    (5, createPropositionWithIdBetween 21 10000)]

createPropositionWithIdBetween::Int -> Int -> LogicFormulaGenerator
createPropositionWithIdBetween min max = liftM Prop (choose (min, max))

addNegation :: RemainingOperators -> LogicFormulaGenerator
addNegation max = liftM Neg (addArbitraryEquasion (max - 1))

addImplication :: RemainingOperators -> LogicFormulaGenerator
addImplication max = createBinaryLogicOperation Impl max

addEquivilance :: RemainingOperators -> LogicFormulaGenerator
addEquivilance max = createBinaryLogicOperation Equiv max

createBinaryLogicOperation:: BinaryLogicOperation -> RemainingOperators -> LogicFormulaGenerator 
createBinaryLogicOperation binaryLogicOperation max = 
    liftM2 binaryLogicOperation (addArbitraryEquasion $ newBinaryMax max) (addArbitraryEquasion $ newBinaryMax max)

newBinaryMax max = max `div` 2

addDisjunction :: RemainingOperators -> LogicFormulaGenerator
addDisjunction max = createOperationWithList Dsj max

addConjunction :: RemainingOperators -> LogicFormulaGenerator
addConjunction max = createOperationWithList Cnj max

createOperationWithList::MultipleLogicOperation -> RemainingOperators -> LogicFormulaGenerator
createOperationWithList multipleLogicOperation max = frequency [
    (80, liftM multipleLogicOperation (createLogicOperationList max 2 3)),
    (19, liftM multipleLogicOperation (createLogicOperationList max 4 10)),
    (1, liftM multipleLogicOperation (createLogicOperationList max 11 50))]

-- inspired by carls answer in 
-- https://stackoverflow.com/questions/25300551/multiple-arbitrary-calls-returning-same-value
createLogicOperationList maxRemaining minLength maxLength = oneof $ 
    [replicateM actualLength (addArbitraryEquasion (maxRemaining `div` actualLength)) | actualLength <- [minLength..maxLength] ]


-- Test Properties
-- 1) test if the truth tables of the input formula and the output CNF is the equivalent

-- *(+(-1 (7<=>4) *(6 20)))