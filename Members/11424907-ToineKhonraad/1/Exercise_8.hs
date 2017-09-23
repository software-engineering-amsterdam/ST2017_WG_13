module Exercise_8 where

import Data.List 

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]


xor :: Bool -> Bool -> Bool
a `xor` b = (a || b) && not ( a && b )

isSaidToLieAbout :: Boy -> Boy -> Bool
isSaidToLieAbout accuser accused = not ( accuses accuser accused  )

-- who accuses who
accuses :: Boy -> Boy -> Bool
accuses Matthew Carl      = False
accuses Matthew Matthew   = False
accuses Matthew _         = True 
accuses Peter   x         = x == Matthew || x == Jack
accuses Jack    boy       = (isSaidToLieAbout Matthew boy ) && (isSaidToLieAbout Peter boy)
accuses Arnold  boy       = not (isSaidToLieAbout Matthew boy ) `xor` (isSaidToLieAbout Peter boy)  
accuses Carl    boy       = isSaidToLieAbout Arnold boy

-- by whom is a boy being accused
accusers :: Boy -> [Boy]
accusers accused = [ boy | boy <- boys, accuses boy accused]  

accusations :: [ ( Boy, [Boy] ) ]                        
accusations = [ (boy, accusers boy) |  boy <- boys ]

{-

  This gives us: (output beautified)
  
  accusations 
  [ ( Matthew, [ Peter,Carl ]           ),
    ( Peter,   [ Matthew,Carl ]         ),
    ( Jack,    [ Matthew,Peter,Arnold ] ),
    ( Arnold,  [ Matthew,Carl ]         ),
    ( Carl,    [ Jack,Arnold ]          )
  ]
  
-}


{-

  Now, What the teacher says comes down to having an element in accusations 
  that has a boy being accused by exactly three others 
-}

thief :: Boy
thief = fst $ head $ filter ( \x -> length ( snd x ) == 3 ) accusations 

honest :: [Boy]
honest = snd $ head $ filter ( \x -> length ( snd x ) == 3 ) accusations  

{-

  This gives us: (output beautified)
  
  thief
    Jack
    
  honest
    [Matthew,Peter,Arnold]

-}

{- Time taken: 3 hours -}

