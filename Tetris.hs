{-
Creator: Lake Giffen-Hunter
Email  : sgiffenh@haverford.edu

Sources:
tetris.wikia.com -- very helpful!!

http://stackoverflow.com/questions/2173628/haskell-ambiguous-occurrences-how-to-avoid
    -import qualified
-}

module Tetris where

import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid


stageWidth, stageHeight :: Int
stageWidth = 300
stageHeight = 500

stageWidthF, stageHeightF :: Float
stageWidthF = fromIntegral stageWidth
stageHeightF = fromIntegral stageHeight

tileR = 10
--this means piece segements need to be 10 apart

tilePath = [ (-tileR / 2, tileR / 2)
             , (-tileR / 2, -tileR / 2)
             , (tileR / 2, -tileR / 2)
             , (tileR / 2, tileR / 2) ]

pieceSpeed :: Float
pieceSpeed = 1

data Piece = Piece { p_color :: Color
                   , p_position :: [Point]  -- rotation piece is always at start of list
                   , p_center :: Point}

initialPiece = Piece { p_color = blue
                     , p_position = [(150,480),(150,490),(160,480),(140,480)]
                     , p_center = (150,480)}

rotatePiece :: Piece -> Piece
rotatePiece p@(Piece { p_color = col
                   , p_position = [(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)]
                   , p_center = (p_cenX, p_cenY)} )
    | (p_cenX == t0X && p_cenY == t0Y) = rotateNormal p
    | otherwise                        = rotateSpecial p

rotateNormal :: Piece -> Piece
rotateNormal p@(Piece { p_color = col
                   , p_position = [t0,t1,t2,t3]
                   , p_center = (p_cenX, p_cenY)} )
    = Piece { p_color = col
            , p_position = [t0, rotateT t1 t0, rotateT t2 t0, rotateT t3 t0]
            , p_center = (p_cenX, p_cenY)}

rotateSpecial :: Piece -> Piece
rotateSpecial p = p

rotateT :: Point -> Point -> Point
rotateT (x,y) (cen_x, cen_y) | (x == cen_x && y > cen_y) -- x+, y-
                                    = (x + abs ((cen_x-x)+(cen_y-y)), y - abs ((cen_x-x)+(cen_y-y)))
                             | (x > cen_x && y == cen_y) -- x-, y-
                                    = (x - abs ((cen_x-x)+(cen_y-y)), y - abs ((cen_x-x)+(cen_y-y)))
                             | (x == cen_x && y < cen_y) -- x-, y+
                                    = (x - abs ((cen_x-x)+(cen_y-y)), y + abs ((cen_x-x)+(cen_y-y)))
                             | otherwise                 -- x+, y+
                                    = (x + abs ((cen_x-x)+(cen_y-y)), y + abs ((cen_x-x)+(cen_y-y)))


data World = World { w_playing :: Bool
                   , w_piece :: Piece }

initialWorld = World { w_playing = False
                     , w_piece = initialPiece }

render :: World -> Picture
render (World { w_piece = Piece { p_color = col
                                , p_position = [(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)]
                                , p_center = (p_cenX, p_cenY)} })
    = translate (-stageWidthF / 2) (-stageHeightF / 2) $
    (translate t0X t0Y $  -- tile 0
     color col $
     polygon tilePath) <>
    (translate t1X t1Y $  -- tile 1
     color col $
     polygon tilePath) <>
    (translate t2X t2Y $  -- tile 2
     color col $
     polygon tilePath) <>
    (translate t3X t3Y $  -- tile 3
     color col $
     polygon tilePath)


step :: Float -> World -> World
step _ w = w
--step _ w@(World { w_playing = False }) = w

react :: Event -> World -> World
react (EventKey (SpecialKey KeySpace) Down _ _)
      w@(World { w_playing = False })
  = w { w_playing = True }
react (EventKey (SpecialKey KeySpace) Down _ _)
      w@(World { w_playing = True
               , w_piece = piece })
  = w { w_piece = rotatePiece piece }
react _ w = w

main :: IO ()
main = play (InWindow "Tetris" (stageWidth, stageHeight) (200, 200))
            white
            50
            initialWorld
            render
            react
            step

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
