module Lab2_Exercise1 where
 
import System.Random

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1) 
            return (p:ps)
            
f :: [Float] -> (Int, Int, Int, Int) ->  (Int, Int, Int, Int)

f []     (a,b,c,d)  = (a,b,c,d)
f (x:xs) (a,b,c,d) 
      | x  < 0.25 =  f xs (a+1, b  , c  , d   ) 
      | x  < 0.50 =  f xs (a  , b+1, c  , d   ) 
      | x  < 0.75 =  f xs (a  , b  , c+1, d   ) 
      | otherwise =  f xs (a  , b  , c  , d+1 )

main = do
   xs <- probs 1000000
   do 
      return (f xs (0,0,0,0) )
  