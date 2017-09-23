module Exercise_5 where
  
import Lecture3

import Exercise_3

normalForm :: Form -> Form
normalForm = (nnf.arrowfree) 


type Clause  = [ Int    ]
type Clauses = [ Clause ]

preConditionError = error "Precondition not met."

{- 
    Precondition: Prop Name >= 1 
-}
toProperty :: Form -> Int
toProperty ( Prop x )         = if (x < 1) then preConditionError else x
toProperty ( Neg ( Prop x ) ) = if (x < 1) then preConditionError else -x
toProperty p@_                  = error $ "syntax in form: " ++ show p

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

-- define cnf2cls as that is the required name for the diliveable
cnf2cls = toClauses

