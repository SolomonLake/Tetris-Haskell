

module Piece where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Line
import System.Random
import System.IO.Unsafe
import Control.Monad
import Data.Array.IO

import GameData


-- TILE STUFF 

data Tile = Tile { t_color :: Color
                 , t_position :: Point }

tileS :: Float
tileS = 30
--this is how far piece segements need to be apart

tileShow :: Float
tileShow = 2.45

tileSInt :: Int
tileSInt = round tileS

tilePath = [ (-tileS / 2 +tileShow, tileS / 2 -tileShow)
             , (-tileS / 2 +tileShow, -tileS / 2 +tileShow)
             , (tileS / 2 -tileShow, -tileS / 2 +tileShow)
             , (tileS / 2 -tileShow, tileS / 2 -tileShow) ] 




-- PIECE DECLARATION STUFF

data Piece = Piece { p_color :: Color
                   , p_position :: [Point]  -- rotation piece is always at start of list
                   , p_center :: Point}


i_Shape = Piece { p_color = (cyan)
                     , p_position = [(105,495),(135,495),(165,495),(195,495)]
                     , p_center = (150,480)}

j_Shape = Piece { p_color = (makeColorI 30 144 255 255)
                     , p_position = [(135,495),(105,495),(165,465),(165,495)]
                     , p_center = (135,495)}
 






l_Shape = Piece { p_color = (makeColorI 255 140 0 255)
                     , p_position = [(135,495),(105,495),(105,465),(165,495)]
                     , p_center = (135,495)}

o_Shape = Piece { p_color = (yellow)
                     , p_position = [(135,465),(135,495),(165,465),(165,495)]
                     , p_center = (150,480)}

s_Shape = Piece { p_color = (green)
                     , p_position = [(135,465),(135,495),(165,495),(105,465)]
                     , p_center = (135,465)}

t_Shape = Piece { p_color = magenta
                     , p_position = [(135,465),(135,495),(165,465),(105,465)]
                     , p_center = (135,465)}

z_Shape = Piece { p_color = (light red)
                     , p_position = [(135,465),(135,495),(165,465),(105,495)]
                     , p_center = (135,465)}

shapeList :: [Piece]
shapeList = [i_Shape,j_Shape,l_Shape,o_Shape,s_Shape,t_Shape,z_Shape]


-- SHUFFLE BAG! THIS METHOD CREATES AN EVEN DISTRIBUTION OF SHAPES

-- | Randomly shuffle a list
--   /O(N)/
-- copied directly from: https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

shapeShuffleBag :: [Piece]
shapeShuffleBag = [i_Shape,j_Shape,l_Shape,o_Shape,s_Shape,t_Shape,z_Shape
                  ,i_Shape,j_Shape,l_Shape,o_Shape,s_Shape,t_Shape,z_Shape
                  ,i_Shape,j_Shape,l_Shape,o_Shape,s_Shape,t_Shape,z_Shape]

getNextBag :: [Piece] -> [Piece]
getNextBag [] = unsafePerformIO (shuffle shapeShuffleBag)
getNextBag [p] = unsafePerformIO (shuffle shapeShuffleBag)
getNextBag ps = tail ps

getNextPiece :: [Piece] -> Piece
getNextPiece [] = head (unsafePerformIO (shuffle shapeShuffleBag))
getNextPiece ps = head (ps)


-- NOW SWITCHED TO SHUFFLE BAG, NO LONGER USING PICK, 
-- pick :: [a] -> a
-- pick [] = error "no items to pick"
-- pick as = pickHelp as (getRandNumLess (length as))

-- getRandNumLess :: Int -> Int
-- getRandNumLess i 
--     | randN < i = randN
--     | otherwise = getRandNumLess i
--     where
--         randN = unsafePerformIO (getStdRandom (randomR (0, i)))

-- pickHelp :: [a] -> Int -> a
-- pickHelp [] _ = error "picking num out of list range" --this sometimes happens..random..
-- pickHelp as 0 = head (as)
-- pickHelp as i = pickHelp (tail as) (i-1)



{-
Pieces:
T:
 @
@X@

I:
@@@@
rotates around center of 4x4 array

O:
@@
@@

S:
 @@
@X

Z:
@@
 X@
 
J:
@
@X@

L:
  @
@X@


-}