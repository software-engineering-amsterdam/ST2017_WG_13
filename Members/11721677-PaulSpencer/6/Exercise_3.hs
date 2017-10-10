module Exercise_3

where 

import Data.List
import System.Random
import Lecture6

composites' :: [Integer]
composites' = filter (not . prime) [3..]
