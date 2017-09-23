x = formGen 5
-- 2 <- lenGen
-- fs' <- formsGen 5 (min s len)
-- con' <- elements [Dsj, Cnj]





-- len <- lenGen
-- fs' <- formsGen s (min s len)
-- con' <- elements [Dsj, Cnj]

formsGen s 1 = do
    f <- formGen (s-1)
    return [f]  
formsGen s len = do
    rn <- choose (0, s)
    fs <- formsGen (s-rn) (len-1)
    f <- formGen (rn-1)
    return (f : fs)