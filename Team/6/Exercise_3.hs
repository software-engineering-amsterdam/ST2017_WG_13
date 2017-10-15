module Exercise_3

where 

import Data.List
import System.Random
import Lecture6_Refactored

-- time taken: 10 mins

-- simply filter the infinite list by the negation 
-- of the prime property from Lecture6.hs.
composites' :: [Integer]
composites' = filter (not . prime) [4..]
