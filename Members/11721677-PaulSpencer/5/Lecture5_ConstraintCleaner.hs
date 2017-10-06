














        
-- constraint cleaners

-- cleanconstraints::[Constraint] -> [Constraint]
-- cleanconstraints cs = chs $ cnd $ cns cs
--   where 
--     cns cs' = if (hasNkdSngl cs') then cleanNakedSingles cs' else cs'
--     cnd cs' = if (hasNkdDbl cs') then cleanNakedDoubles cs' else cs'
--     chs cs' = if (hasHdnSngl cs') then cleanHiddenSingles cs' else cs'

-- cleanHiddenSingles::[Constraint] -> [Constraint]
-- cleanHiddenSingles constraints = map (cleanHiddenSingle constraints) constraints

-- cleanHiddenSingle::[Constraint] -> Constraint -> Constraint
-- cleanHiddenSingle constraints (loc, vs) = (loc, (if null ins then vs else ins))
--     where
--       ins = vs `intersect` (hiddenSingles constraints loc)

-- hiddenSingles :: [Constraint] -> Location -> [Value]
-- hiddenSingles constraints loc = concat $ map fndHdnSngl $ ((getGroupedConstraintsForLocIncl) constraints loc)
-- --otherHiddenSingles = undefined

-- d1::[Value]
-- d1 = [1,2]
-- d2 = [3,4]
-- d3 = [5,6,7]
-- d4 = [2,5,6,7,9]
-- d5 = [1..9]
-- d5' = [1..7]++[9]
-- cons::[Constraint]
-- cons = zip alllocs (d1:d2:d1:d3:d4:d5:d2:d4:d3:
--                     d1:d2:d1:d3:d4:d5:d2:d4:d3:(repeat []))


-- fndHdnSngl::[Constraint] -> [Value]
-- fndHdnSngl constraints = map fst $ filter (\(_,n) -> n == 1) $ count $ concat $  map snd constraints

-- hasHdnSngl::[Constraint] -> Bool
-- hasHdnSngl constraints = (length (fndHdnSngl constraints)) > 1


-- count :: Ord a => [a] -> [(a, Int)]
-- count = map lh . sg
-- lh :: [a] -> (a, Int)
-- lh = liftA2 (,) head length
-- sg :: Ord a => [a] -> [[a]]
-- sg = group . sort

-- cleanNakedSingles::[Constraint] -> [Constraint]
-- cleanNakedSingles constraints = map (cleanNakedSingle constraints) constraints

-- cleanNakedSingle::[Constraint] -> Constraint -> Constraint
-- cleanNakedSingle constraints (loc, vs) = (loc, (vs \\ (otherNakedSingles constraints loc)))

-- otherNakedSingles :: [Constraint] -> Location -> [Value]
-- otherNakedSingles constraints loc = fndNkdSngl $ (getAllConstraintsForLoc constraints loc)

-- fndNkdSngl::[Constraint] -> [Value]
-- fndNkdSngl constraints = concat $ filter (\x -> length x == 1) $ map snd constraints 
-- hasNkdSngl constraints = (length (fndNkdSngl constraints)) > 1

-- cleanNakedDoubles::[Constraint] -> [Constraint]
-- cleanNakedDoubles constraints = map (cleanNakedDouble constraints) constraints

-- cleanNakedDouble::[Constraint] -> Constraint -> Constraint
-- cleanNakedDouble constraints (loc, vs) = (loc, vs \\ otherNakedDoubles constraints loc)

-- otherNakedDoubles :: [Constraint] -> Location -> [Value]
-- otherNakedDoubles constraints loc = concat $ concat $ map fndNkdDbl (getGroupedConstraintsForLoc constraints loc)

-- hasNkdDbl constraints = (length (fndNkdDbl constraints)) > 1
-- fndNkdDbl constraints = fndlen2 \\ (unique fndlen2)  
--   where 
--     fndlen2 =  filter (\x -> length x == 2) $ map snd constraints

-- filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
-- filterByLength p = filter (p . length) . sg

-- unique :: Ord a => [a] -> [a]
-- unique = concat . filterByLength (==1)

-- getAllConstraintsForLoc::[Constraint] -> Location -> [Constraint]
-- getAllConstraintsForLoc constraints loc =  filter (consFilter (getAllLocsForLoc loc)) constraints

-- getGroupedConstraintsForLoc::[Constraint] -> Location -> [[Constraint]]
-- getGroupedConstraintsForLoc constraints loc = map (map (cons4Loc constraints)) (getGroupedLocsForLoc loc)

-- getGroupedConstraintsForLocIncl::[Constraint] -> Location -> [[Constraint]]
-- getGroupedConstraintsForLocIncl constraints loc = map (map (cons4Loc constraints)) (getGroupedLocsForLocIncl loc)


-- cons4Loc::[Constraint] -> Location -> Constraint
-- cons4Loc [] loc = (loc, []) -- this probably is not the right thing to do
-- cons4Loc (cs@(loc',_):xs) loc 
--   | loc == loc' = cs
--   | otherwise   = cons4Loc xs loc

-- consFilter::[Location] -> Constraint -> Bool
-- consFilter locs (loc,_) = loc `elem` locs

-- getAllLocsForLoc::Location -> [Location]
-- getAllLocsForLoc loc@(r,c) = (nub $ locsInRow r ++ locsInCol c ++ (concat $ subgridsForLoc loc)) \\ [loc]

-- getGroupedLocsForLoc::Location -> [[Location]]
-- getGroupedLocsForLoc loc@(r,c) = map (filter (/=loc)) $ getGroupedLocsForLocIncl loc
-- --getGroupedLocsForLoc = undefined

-- getGroupedLocsForLocIncl::Location -> [[Location]]
-- getGroupedLocsForLocIncl loc@(r,c) = ([locsInRow r] ++ [locsInCol c] ++ (subgridsForLoc loc))












--getconstraints valFinder =  (sortByConstraintVals $ cleanconstraints allConstraints) 
--emptySnapShot = (\_ -> 0, getconstraints (\_ -> 0))

