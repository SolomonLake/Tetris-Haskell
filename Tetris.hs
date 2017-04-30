{-
Creator: Lake Giffen-Hunter
Email  : sgiffenh@haverford.edu

Sources:
tetris.wikia.com -- very helpful!!

http://stackoverflow.com/questions/2173628/haskell-ambiguous-occurrences-how-to-avoid
    -import qualified
-}

module Tetris where

import Data.Maybe
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Line
import Data.Monoid
import System.Random
import System.IO.Unsafe

-- GAME DATA

stageWidth, stageHeight :: Int
stageWidth = 300
stageHeight = 540
-- TODO make these scalable from tileS, then incorporate this into initial piece pos

stageWidthF, stageHeightF :: Float
stageWidthF = fromIntegral stageWidth
stageHeightF = fromIntegral stageHeight

tileS :: Float
tileS = 30
--this means piece segements need to be 10 apart

tilePath = [ (-tileS / 2, tileS / 2)
             , (-tileS / 2, -tileS / 2)
             , (tileS / 2, -tileS / 2)
             , (tileS / 2, tileS / 2) ] 

pieceSpeed :: Float
pieceSpeed = 1

data Piece = Piece { p_color :: Color
                   , p_position :: [Point]  -- rotation piece is always at start of list
                   , p_center :: Point}

initialPiece :: Piece
initialPiece = o_Shape--pick shapeList

pick :: [a] -> a
pick [] = error "no items to pick"
pick as = pickHelp as (unsafePerformIO (getStdRandom (randomR (1, length as))))

pickHelp :: [a] -> Int -> a
pickHelp [] _ = error "picking num out of list range"
pickHelp as 0 = head (as)
pickHelp as i = pickHelp (tail as) (i-1)

i_Shape = Piece { p_color = cyan
                     , p_position = [(105,495),(135,495),(165,495),(195,495)]
                     , p_center = (150,480)}

j_Shape = Piece { p_color = blue
                     , p_position = [(135,495),(105,495),(165,465),(165,495)]
                     , p_center = (135,495)}

l_Shape = Piece { p_color = orange
                     , p_position = [(135,495),(105,495),(105,465),(165,495)]
                     , p_center = (135,495)}

o_Shape = Piece { p_color = yellow
                     , p_position = [(135,465),(135,495),(165,465),(165,495)]
                     , p_center = (150,480)}

s_Shape = Piece { p_color = green
                     , p_position = [(135,465),(135,495),(165,495),(105,465)]
                     , p_center = (135,465)}

t_Shape = Piece { p_color = violet
                     , p_position = [(135,465),(135,495),(165,465),(105,465)]
                     , p_center = (135,465)}

z_Shape = Piece { p_color = red
                     , p_position = [(135,465),(135,495),(165,465),(105,495)]
                     , p_center = (135,465)}

shapeList :: [Piece]
shapeList = [i_Shape,j_Shape,l_Shape,o_Shape,s_Shape,t_Shape,z_Shape]

leftWall :: Path
leftWall = [(0,0),(0,stageHeightF)]

rightWall :: Path
rightWall = [(stageWidthF,0),(stageWidthF,stageHeightF)]


-- GAME FUNCTIONS

bottomOut :: Piece -> [Float] -> Bool
bottomOut p ls = bottomOutRec (nextPosStep p) ls

bottomOutRec :: [Point] -> [Float] -> Bool
bottomOutRec _ [] = False
bottomOutRec ps@([(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)] ) ls
    | (t0Y < head ls || t1Y < head ls || t2Y < head ls || t3Y < head ls) 
                = True
    | otherwise = bottomOutRec ps (tail ls)

nextPosStep :: Piece -> [Point]
nextPosStep Piece { p_position = [(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)] }
    =  [(t0X+(0*tileS),t0Y+(-1*tileS)),(t1X+(0*tileS),t1Y+(-1*tileS))
       ,(t2X+(0*tileS),t2Y+(-1*tileS)),(t3X+(0*tileS),t3Y+(-1*tileS)) ]

pathsOverlap :: Path -> Path -> Bool
pathsOverlap [a1,a2] [b1,b2]
    = case intersectSegSeg a1 a2 b1 b2 of
        Nothing -> False
        Just (p) -> True

notOverlap :: [Point] -> Bool
notOverlap pos = notOverlapWalls pos && notOverlapFloor pos
-- need to also not overlap with placed pieces :)

notOverlapFloor :: [Point] -> Bool
notOverlapFloor [(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)]
    | (t0Y < 0 || t1Y < 0 || t2Y < 0 || t3Y < 0) 
                = False
    | otherwise = True
notOverlapFloor _ = error "not a piece position"

notOverlapWalls :: [Point] -> Bool
notOverlapWalls [(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)]
    |      (t0X < 0 || t1X < 0 || t2X < 0 || t3X < 0) 
        || (t0X > stageWidthF || t1X > stageWidthF
                || t2X > stageWidthF || t3X > stageWidthF)
                = False
    | otherwise = True
notOverlapWalls _ = error "not a piece position"


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
    | notOverlap nPos = Piece { p_color = col
                                 , p_position = nPos
                                 , p_center = (p_cenX, p_cenY)}
    | otherwise = p
        where
            nPos = [t0, rotateT t1 t0, rotateT t2 t0, rotateT t3 t0]

rotateT :: Point -> Point -> Point
rotateT (x,y) (cen_x, cen_y) | (x == cen_x && y > cen_y) -- x+, y-
                                    = (x + (abs(cen_x-x)+ abs(cen_y-y)), y - (abs(cen_x-x)+ abs(cen_y-y)))
                             | (x > cen_x && y == cen_y) -- x-, y-
                                    = (x - (abs(cen_x-x)+ abs(cen_y-y)), y - (abs(cen_x-x)+ abs(cen_y-y)))
                             | (x == cen_x && y < cen_y) -- x-, y+
                                    = (x - (abs(cen_x-x)+ abs(cen_y-y)), y + (abs(cen_x-x)+ abs(cen_y-y)))
                             | (x < cen_x && y == cen_y) -- x+, y+
                                    = (x + (abs(cen_x-x)+ abs(cen_y-y)), y + (abs(cen_x-x)+ abs(cen_y-y)))
                             | (x < cen_x && y < cen_y)  -- x, y++
                                    = (x, y + (abs(cen_x-x)+ abs(cen_y-y)))
                             | (x < cen_x && y > cen_y)  -- x++, y
                                    = (x + (abs(cen_x-x)+ abs(cen_y-y)), y)
                             | (x > cen_x && y > cen_y)  -- x, y--
                                    = (x, y - (abs(cen_x-x)+ abs(cen_y-y)))
                             | (x > cen_x && y < cen_y)  -- x++, y
                                    = (x - (abs(cen_x-x)+ abs(cen_y-y)), y)
                             | otherwise = error "weird shape"

rotateSpecial :: Piece -> Piece
rotateSpecial p@(Piece { p_color = col
                              , p_position = [(t0X,t0Y),t1,t2,t3]
                              , p_center = pc@(p_cenX, p_cenY)} )
    | (abs (p_cenX - t0X) > tileS) || (abs (p_cenY - t0Y) > tileS)
        = case notOverlap nPos of
            False -> p
            True -> Piece { p_color = col
                                 , p_position = nPos
                                 , p_center = (p_cenX, p_cenY)}
    | otherwise = p
        where
            nPos = [rotateFarST (t0X,t0Y) pc, rotateCloseST t1 pc 
                  , rotateCloseST t2 pc, rotateFarST t3 pc]

rotateCloseST :: Point -> Point -> Point
rotateCloseST (x,y) (cen_x, cen_y) | (x > cen_x) && (y > cen_y)
                                        = (x, y - tileS)
                                   | (x > cen_x) && (y < cen_y)
                                        = (x - tileS, y)
                                   | (x < cen_x) && (y < cen_y)
                                        = (x, y + tileS)
                                   | (x < cen_x) && (y > cen_y)
                                        = (x + tileS, y)
                                   | otherwise = error "weird shape"

rotateFarST :: Point -> Point -> Point
rotateFarST (x,y) (cen_x, cen_y) 
            | (x > cen_x) && (y > cen_y)
                 = case abs(cen_x - x) > tileS of
                    True -> (x - tileS, y - (tileS*2))
                    False -> (x + tileS, y - (tileS*2))
            | (x > cen_x) && (y < cen_y)
                 = case abs(cen_x - x) > tileS of
                    True -> (x - (tileS*2), y - tileS)
                    False -> (x - (tileS*2), y + tileS)
            | (x < cen_x) && (y < cen_y)
                 = case abs(cen_x - x) > tileS of
                    True -> (x + tileS, y + (tileS*2))
                    False -> (x - tileS, y + (tileS*2))
            | (x < cen_x) && (y > cen_y)
                 = case abs(cen_x - x) > tileS of
                    True -> (x + (tileS*2), y + tileS)
                    False -> (x + (tileS*2), y - tileS)
            | otherwise = error "weird shape"
    {-False ->| (x > cen_x) && (y > cen_y)
                 = (x + tileS, y - (tileS*2))
            | (x > cen_x) && (y < cen_y)
                 = (x - (tileS*2), y + tileS)
            | (x < cen_x) && (y < cen_y)
                 = (x - tileS, y + (tileS*2))
            | (x < cen_x) && (y > cen_y)
                 = (x + (tileS*2), y - tileS)
            | otherwise = error "weird shape"
            -}



translatePiece :: Point -> Piece -> Piece
translatePiece (x,y) p@(Piece { p_color = col
                              , p_position = [(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)]
                              , p_center = (p_cenX, p_cenY)} )
    | notOverlap nPos = Piece { p_color = col
                                , p_position = nPos
                                , p_center = (p_cenX+(x*tileS), p_cenY+(y*tileS))}
    | otherwise = p
        where
            nPos = [(t0X+(x*tileS),t0Y+(y*tileS)),(t1X+(x*tileS),t1Y+(y*tileS))
                   ,(t2X+(x*tileS),t2Y+(y*tileS)),(t3X+(x*tileS),t3Y+(y*tileS)) ]
    


-- WORLD AND ITS FUNCTIONS

data World = World { w_playing :: Bool
                   , w_piece :: Piece
                   , w_bottom :: [Float] }

initialWorld = World { w_playing = False
                     , w_piece = initialPiece
                     , w_bottom = replicate 10 0 }

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
     polygon tilePath) <>
    (translate t0X t0Y $  -- tile 0 BLACK FRAME
     color black $
     rectangleWire tileS tileS) <>
    (translate t1X t1Y $  -- tile 1
     color black $
     rectangleWire tileS tileS) <>
    (translate t2X t2Y $  -- tile 2
     color black $
     rectangleWire tileS tileS) <>
    (translate t3X t3Y $  -- tile 3
     color black $
     rectangleWire tileS tileS)


step :: Float -> World -> World
step _ w@(World { w_playing = False }) = w
step _ w@(World { w_piece = piece
                , w_bottom = botLs})
    | bottomOut piece botLs = error "do piece landing stuff here :)"
    | otherwise = w { w_piece = translatePiece (0,-1) piece }
-- step _ w = w


react :: Event -> World -> World
react (EventKey (SpecialKey KeySpace) Down _ _)
      w@(World { w_playing = False })
  = w { w_playing = True }
react (EventKey (SpecialKey KeySpace) Down _ _)
      w@(World { w_playing = True
               , w_piece = piece })
  = w { w_piece = rotatePiece piece }
react (EventKey (SpecialKey KeyRight) Down _ _)
      w@(World { w_playing = True
               , w_piece = piece })
  = w { w_piece = translatePiece (1,0) piece}
react (EventKey (SpecialKey KeyLeft) Down _ _)
      w@(World { w_playing = True
               , w_piece = piece })
  = w { w_piece = translatePiece (-1,0) piece}
react _ w = w


main :: IO ()
main = play (InWindow "Tetris" (stageWidth, stageHeight) (200, 200))
            white
            2
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
