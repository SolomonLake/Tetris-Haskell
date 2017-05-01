

module OverlapFuncs where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Line

import Piece
import GameData


-- PIECE OVERLAP FUNCS

nextPosStep :: Piece -> [Point]
nextPosStep Piece { p_position = [(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)] }
    =  [(t0X+(0*tileS),t0Y+(-1*tileS)),(t1X+(0*tileS),t1Y+(-1*tileS))
       ,(t2X+(0*tileS),t2Y+(-1*tileS)),(t3X+(0*tileS),t3Y+(-1*tileS)) ]

notOverlap :: [Tile] -> [Point] -> Bool
notOverlap ts pos = notOverlapWalls pos && notOverlapFloor pos && notOverlapPlacedTs ts pos

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



notOverlapPiece :: Piece -> [Point] -> Bool
notOverlapPiece Piece{ p_position = [pt0,pt1,pt2,pt3] } [t0,t1,t2,t3]
    | t0 == pt0 || t0 == pt1 || t0 == pt2 || t0 == pt3 ||
      t1 == pt0 || t1 == pt1 || t1 == pt2 || t1 == pt3 ||
      t2 == pt0 || t2 == pt1 || t2 == pt2 || t2 == pt3 ||
      t3 == pt0 || t3 == pt1 || t3 == pt2 || t3 == pt3
                = False
    | otherwise = True

notOverlapPlacedTs :: [Tile] -> [Point] -> Bool
notOverlapPlacedTs [] _ = True
notOverlapPlacedTs ts pos 
    | notOverlapTile (head ts) pos = notOverlapPlacedTs (tail ts) pos
    | otherwise = False

notOverlapTile :: Tile -> [Point] -> Bool
notOverlapTile Tile { t_position = tpos } [t0,t1,t2,t3]
    | tpos == t0 || tpos == t1 || tpos == t2 || tpos == t3
                = False
    | otherwise = True