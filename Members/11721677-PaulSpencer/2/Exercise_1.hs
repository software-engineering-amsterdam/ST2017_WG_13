module Lab2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Your programmer Red Curry has written the following function for generating lists of floating point numbers.

-- > probs :: Int -> IO [Float]
-- > probs 0 = return []
-- > probs n = do
-- >              p <- getStdRandom random
-- >              ps <- probs (n-1) 
-- >              return (p:ps)
-- He claims that these numbers are random in the open interval (0..1)(0..1). Your task is to test whether 
-- this claim is correct, by counting the numbers in the quartiles

-- (0..0.25),[0.25..0.5),[0.5..0.75),[0.75..1)(0..0.25),[0.25..0.5),[0.5..0.75),[0.75..1)

-- and checking whether the proportions between these are as expected.

-- E.g., if you generate 10000 numbers, then roughly 2500 of them should be in each quartile.

-- Implement this test, and report on the test results.

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)