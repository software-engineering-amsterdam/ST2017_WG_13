module Exercise_3 where
import Data.List
import Lecture3
import Test.QuickCheck

import Exercise_1

truthTable formula = allVals formula

falseValuations :: Form -> [Valuation]

falseValuations formula = [ row | row <- truthTable formula, not (evl row formula) ]

valToProp :: (Name, Bool) -> Form
valToProp (name, bool) = if bool then (Neg (Prop name)) else (Prop name)

valToForm :: Valuation -> Form
valToForm valuation = Dsj [ valToProp x | x <- valuation ]

valsToForm :: [Valuation] -> Form
valsToForm  =  Cnj . map valToForm 

formToCnf formula = valsToForm $ falseValuations formula

