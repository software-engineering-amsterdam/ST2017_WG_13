-- Recognizing and generating derangements

-- A derangement of the list [0..n-1] of natural numbers is a permutation π of the list with the property 
-- that for no xx in the list π(x)=xπ(x)=x. This is what you need if you prepare for Sinterklaas with a 
-- group of friends, where you want to avoid the situation that someone has to buy a surprise gift for him- 
-- or herself.

-- Give a Haskell implementation of a property isDerangement that checks whether one list is a derangement 
-- of another one.

-- Give a Haskell implementation of a function deran that generates a list of all derangements of the list 
-- [0..n-1].

-- Note You may wish to use the permutations function from Data.List, or the perms function from workshop 1.

-- Next, define some testable properties for the isDerangement function, and use some well-chosen integer 
-- lists to test isDerangement.

-- Provide an ordered list of properties by strength using the weakear and stronger definitions.

-- Can you automate the test process?

-- Deliverables: Haskell program, concise test report, indication of time spent.