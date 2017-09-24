module Exercise_3 where
    import Data.List
    import Lecture3
    import Test.QuickCheck
    
    import Exercise_1
    
    truthTable :: Form -> [Valuation]
    truthTable f = allVals f
    
    falseValuations :: Form -> [Valuation]
    falseValuations formula = [ row | row <- truthTable formula, not (evl row formula) ]
    
    valToProp :: (Name, Bool) -> Form
    valToProp (name, bool) = if bool then (Neg (Prop name)) else (Prop name)
    
    valToForm :: Valuation -> Form
    valToForm valuation = Dsj [ valToProp x | x <- valuation ]
    
    valsToForm :: [Valuation] -> Form
    valsToForm  =  Cnj . map valToForm 

    -- Optimised for Tautologies, see "Exercise_3.md"
    formToCnf :: Form -> Form
    formToCnf form | tautology form = Cnj []
                   | otherwise      = valsToForm $ falseValuations form 

    --- Testing Procedure ---

    --- Create List of Properties ---
    a = Prop 1
    b = Prop 2
    c = Prop 3
    d = Prop 4
    e = Prop 5
    g = Prop 6
    

    --- Form List to test On ---
    testInput :: [Form]
    testInput = [
      form1,
      form2,
      form3,
      Cnj[a,Neg a],
      Dsj[a,Neg a],
      Dsj[a],
      Cnj[b],
      Dsj[a,b],
      Cnj[a,b,c,d,e,g],
      Impl a b,
      Impl b a,
      Equiv c d,
      Dsj[Impl a b, Impl c d],
      Impl (Cnj [a]) (Dsj[b,c,d])
      ]    

    cnfTest :: Form -> Bool
    cnfTest x = equiv x (formToCnf x)

    --- Run showing the resulting cnf forms of the testInput forms ---
    detailedRun = map formToCnf testInput

    --- Run showing only the truth value of the comparison of the form and its cnf conversion ---
    run = map cnfTest testInput