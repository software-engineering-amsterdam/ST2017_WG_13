module Exercise_5 where
  
import Lecture3

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

toClause :: Form -> Clause
toClause ( Dsj list )         = map toProperty list
toClause formTerm             = toClause $ Dsj [formTerm] 


toClauses :: Form -> Clauses
toClauses ( Equiv _ _       ) = preConditionError
toClauses ( Impl _ _        ) = preConditionError
toClauses ( Cnj clauseList  ) = map toClause clauseList
toClauses formTerm            = toClauses $ Cnj [formTerm]

{- 
    Precondition: Form in CNF (nnf and arrowfree and prop names >= 1)
-}
cnf2cls :: Form -> Clauses
cnf2cls = toClauses


