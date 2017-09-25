module Exercise_5 where
  
import Test.QuickCheck

import Lecture3

import Exercise_1
import Exercise_3
import Exercise_4

{- 
   For notes on this exercise see:
   https://github.com/software-engineering-amsterdam/ST2017_WG_13/blob/master/Team/3/Exercise_5.md
-}

type Clause  = [ Int    ]
type Clauses = [ Clause ]

preConditionError = error "Precondition not met."

{- 
    Precondition: Prop Name >= 1 
-}
toProperty :: Form -> Int
toProperty ( Prop x )         = if (x < 1) then preConditionError else x
toProperty ( Neg ( Prop x ) ) = if (x < 1) then preConditionError else -x
toProperty p@_                = error $ "syntax in form: " ++ show p

toClause :: Form -> Clause
toClause ( Dsj list )         = map toProperty list
toClause formTerm             = toClause $ Dsj [formTerm] 


{- 
    Precondition: Form in CNF (nnf and arrowfree and prop names >= 1)
-}
toClauses :: Form -> Clauses
toClauses ( Equiv _ _       ) = preConditionError
toClauses ( Impl _ _        ) = preConditionError
toClauses ( Cnj clauseList  ) = map toClause clauseList
toClauses formTerm            = toClauses $ Cnj [formTerm]

-- define cnf2cls as that is the required name for the diliverable
cnf2cls = toClauses

-- To test this I went the other way around: clfToForm creates a form out
-- of the CLS, to test the equivalence of that to the orginal Form.
toProp n 
    | n < 0     = Neg (Prop (abs n))
    | otherwise = Prop n

cls2form cs = Cnj (map (\fs -> Dsj ((map toProp) fs)) cs)

prop_equivalent :: Form -> Bool
prop_equivalent form = (formToCnf form)
                       `equiv`
                       (cls2form $ cnf2cls $ formToCnf form)
                       
test_equivalent = verboseCheck prop_equivalent
