{-
Creator: Lake Giffen-Hunter
Email  : sgiffenh@haverford.edu

-}

module Tetris where

import Data.Maybe
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Line
import Data.Monoid
import System.Random
import System.IO.Unsafe

import Piece
import OverlapFuncs
import GameData


-- PIECE ROTATE AND TRANSLATE FUNCS

rotatePiece :: [Tile] -> Piece -> Piece
rotatePiece ts p@(Piece { p_color = col
                   , p_position = [(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)]
                   , p_center = (p_cenX, p_cenY)} )
    | (p_cenX == t0X && p_cenY == t0Y) = rotateNormal ts p
    | otherwise                        = rotateSpecial ts p

rotateNormal :: [Tile] -> Piece -> Piece
rotateNormal ts p@(Piece { p_color = col
                   , p_position = [t0,t1,t2,t3]
                   , p_center = (p_cenX, p_cenY)} )
    | notOverlap ts nPos = Piece { p_color = col
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

rotateSpecial :: [Tile] -> Piece -> Piece
rotateSpecial ts p@(Piece { p_color = col
                              , p_position = [(t0X,t0Y),t1,t2,t3]
                              , p_center = pc@(p_cenX, p_cenY)} )
    | (abs (p_cenX - t0X) > tileS) || (abs (p_cenY - t0Y) > tileS)
        = case notOverlap ts nPos of
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




translatePiece :: [Tile] -> Point -> Piece -> Piece
translatePiece ts (x,y) p@(Piece { p_color = col
                              , p_position = [(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)]
                              , p_center = (p_cenX, p_cenY)} )
    | notOverlap ts nPos = Piece { p_color = col
                                , p_position = nPos
                                , p_center = (p_cenX+(x*tileS), p_cenY+(y*tileS))}
    | otherwise = p
        where
            nPos = [(t0X+(x*tileS),t0Y+(y*tileS)),(t1X+(x*tileS),t1Y+(y*tileS))
                   ,(t2X+(x*tileS),t2Y+(y*tileS)),(t3X+(x*tileS),t3Y+(y*tileS)) ]



-- GENERAL FUNCTIONS FOR GAME STATE

movePieceToBot :: [Tile] -> Piece -> Piece
movePieceToBot ts p@(Piece { p_color = col
                              , p_position = pos@([(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)])
                              , p_center = (p_cenX, p_cenY)} )
    | notOverlap ts (nextPosStep p) --Case where you should move it down one
          = movePieceToBot ts (p { p_position = nextPosStep p})
    | otherwise                     -- Case where we have reached the bottom, return p
          = p

clearCompleteRows :: [Point] -> [Tile] -> [Tile]
--cycle through each row, until stageHeight
clearCompleteRows [] ts = ts
clearCompleteRows ps ts = clearCompleteRows (tail ps) (ccRHelper (getYPoint (head ps)) ts)



getYPoint :: Point -> Int
getYPoint (x,y) = round y

ccRHelper :: Int -> [Tile] -> [Tile]
ccRHelper row ts = if tenOfRow 0 row ts
                      then deleteRow row ts
                   else ts


-- OLDER, LESS OPTIMIZED CODE:
-- clearCompleteRows :: [Tile] -> [Tile]
-- --cycle through each row, until stageHeight
-- clearCompleteRows [] ts = ccRHelper (quot tileSInt 2) ts
--
-- ccRHelper :: Int -> [Tile] -> [Tile]
-- ccRHelper row ts | row > stageHeight = ts
--                  | otherwise = if tenOfRow 0 row ts
--                                   then ccRHelper row (deleteRow row ts)
--                                else ccRHelper (row + (quot tileSInt 2)) ts

tenOfRow :: Int -> Int -> [Tile] -> Bool
tenOfRow 10 _ _ = True
tenOfRow _ _ [] = False
tenOfRow ct row ts | tileYPos (head ts) == row = tenOfRow (ct+1) row (tail ts)
                   | otherwise                 = tenOfRow ct row (tail ts)

tileYPos :: Tile -> Int
tileYPos Tile { t_position = (x,y) } = round y

deleteRow :: Int -> [Tile] -> [Tile]
deleteRow _ [] = []
deleteRow row ts | tileYPos (head ts) == row = deleteRow row (tail ts)
                 | tileYPos (head ts) > row  = [moveTileDown (head ts)] ++ deleteRow row (tail ts)
                 | otherwise                 = [head ts] ++ deleteRow row (tail ts)

moveTileDown :: Tile -> Tile
moveTileDown Tile { t_color = col
                  , t_position = (x,y) }
                  = Tile { t_color = col
                         , t_position = (x,y-(tileS))}


-- WORLD AND GAME FUNCTIONS

data World = World { w_playing :: Bool
                   , w_piece :: Piece
                   --, w_placedPieces :: [Piece]
                   , w_tiles :: [Tile]
                   , w_nextPiece :: Piece
                   , w_startGame :: Bool
                   , w_numPiecesP :: Int
                   , w_prevGameNum :: Int
                   , w_pieceSpeed :: Float
                   , w_pieceBag :: [Piece] }

initialWorld = World { w_playing = False
                     , w_piece = getNextPiece []
                     --, w_placedPieces = []
                     , w_tiles = []
                     , w_nextPiece = getNextPiece []
                     , w_startGame = True
                     , w_numPiecesP = 0
                     , w_prevGameNum = 0
                     , w_pieceSpeed = 1
                     , w_pieceBag = getNextBag [] }



-- RENDER STUFF

render :: World -> Picture
render (World { w_playing = playing
              , w_piece = p@(Piece { p_color = col
                                , p_position = [(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)]
                                , p_center = (p_cenX, p_cenY)} )
              --, w_placedPieces = ps
              , w_tiles = ts
              , w_nextPiece = np@(Piece { p_color = n_col
                                , p_position = n_pos
                                , p_center = n_cen} )
              , w_startGame = sGame
              , w_numPiecesP = numP
              , w_prevGameNum = prevGameNum })
    | playing == True 
       = (translate ((-1 * stageWidthF) - (stageWidthF/4.1))  (-stageHeightF / 2) $
            renderPiece np) <>
       (translate (stageWidthF - 100)  (stageHeightF / 2 - 70) $
            scale 0.4 0.4 $ text $ show numP) <>
       (translate (-stageWidthF / 2) (-stageHeightF / 2) $
                   (translate (stageWidthF/2) (stageHeightF/2) $  -- tile 0 BLACK FRAME
                    color black $
                    rectangleWire stageWidthF stageHeightF) <>
                   renderPiece p <>
                   renderTileList ts)
    | sGame == True = (translate (-stageWidthF / 2) (stageHeightF/2 -50) $
                          scale 0.2 0.2 $ text $ "Press SPACE to begin") <>
                      (translate (-stageWidthF / 2) (stageHeightF/2 -150) $
                          scale 0.2 0.2 $ text $ "Controls:") <>
                      (translate (-stageWidthF / 2) (stageHeightF/2 -200) $
                          scale 0.2 0.2 $ text $ "Space: Rotate") <>
                      (translate (-stageWidthF / 2) (stageHeightF/2 -250) $
                          scale 0.2 0.2 $ text $ "P: Pause")
    | otherwise      -- end game
                    = (translate (-stageWidthF / 2) (stageHeightF/2 -50) $
                          scale 0.2 0.2 $ text $ "GAME OVER") <>
                      (translate (-stageWidthF / 2) (stageHeightF/2 -150) $
                          scale 0.2 0.2 $ text $ "Press SPACE to play again") <>
                      (translate (-stageWidthF / 2) (stageHeightF/2 -200) $
                          scale 0.2 0.2 $ text $ "Score: " ++ show prevGameNum)

renderPiece :: Piece -> Picture
renderPiece Piece { p_color = col
                                , p_position = [(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)]
                                , p_center = (p_cenX, p_cenY)} =
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



renderTileList :: [Tile] -> Picture
renderTileList [] = blank
renderTileList [t] = renderTile t
renderTileList ts = renderTile (head ts) <> renderTileList (tail ts)

renderTile :: Tile -> Picture
renderTile Tile { t_color = col
                , t_position = (tX, tY)} =
    (translate tX tY $  
     color col $
     polygon tilePath) <>
    (translate tX tY $
     color black $
     rectangleWire tileS tileS)


-- STEP STUFF

step :: Float -> World -> World
step _ w@(World { w_playing = False }) = w
step _ w@(World { w_piece = p@(Piece { p_color = col
                              , p_position = p_pos@([t0,t1,t2,t3])
                              , p_center = pc@(p_cenX, p_cenY)} )
                --, w_placedPieces = placedP
                , w_tiles = ts
                , w_nextPiece = np
                , w_numPiecesP = numP
                , w_pieceSpeed = pSpeed
                , w_pieceBag = pBag })
    | notOverlap ts (nextPosStep p) 
                            = w { w_piece = translatePiece ts (0,-1) p }
                -- this is where key down cases need to be
    | otherwise = case notOverlapPlacedTs ts p_pos of
      True -> w { w_piece = np
                    --, w_placedPieces = [p] ++ placedP
                    , w_tiles = clearCompleteRows 
                                 [t0,t1,t2,t3]
                                ([Tile {t_color = col, t_position = t0}
                                 ,Tile {t_color = col, t_position = t1}
                                 ,Tile {t_color = col, t_position = t2}
                                 ,Tile {t_color = col, t_position = t3}]  ++ ts)
                    , w_nextPiece = head pBag
                    , w_numPiecesP = numP + 1
                    , w_pieceSpeed = fromIntegral (quot numP numPiecesPerLevel) + 2
                    , w_pieceBag = getNextBag pBag }
            -- here is where you check for complete row

      False -> World { w_playing = False
                     , w_piece = getNextPiece []
                     --, w_placedPieces = []
                     , w_tiles = []
                     , w_nextPiece = getNextPiece []
                     , w_startGame = False
                     , w_numPiecesP = 0
                     , w_prevGameNum = numP
                     , w_pieceSpeed = 2
                     , w_pieceBag = getNextBag [] }
-- step _ w = w


-- REACT STUFF

react :: Event -> World -> World
react (EventKey (SpecialKey KeySpace) Down _ _)
      w@(World { w_playing = False })
  = w { w_playing = True }
react (EventKey (SpecialKey KeySpace) Down _ _)
      w@(World { w_playing = True
               , w_piece = piece
               , w_tiles = ts })
  = w { w_piece = rotatePiece ts piece }
react (EventKey (SpecialKey KeyRight) Down _ _)
      w@(World { w_playing = True
               , w_piece = piece
               , w_tiles = ts })
  = w { w_piece = translatePiece ts (1,0) piece }
react (EventKey (SpecialKey KeyLeft) Down _ _)
      w@(World { w_playing = True
               , w_piece = piece
               , w_tiles = ts })
  = w { w_piece = translatePiece ts (-1,0) piece }
react (EventKey (SpecialKey KeyDown) Down _ _)
      w@(World { w_playing = True
               , w_piece = piece
               , w_tiles = ts })
  = w { w_piece = translatePiece ts (0,-1) piece }
react (EventKey (Char 'p') Down _ _)
      w@(World { w_playing = True })
  = w { w_playing = False }
react (EventKey (SpecialKey KeyUp) Down _ _)
      w@(World { w_playing = True
               , w_piece = piece
               , w_tiles = ts })
  = w { w_piece = movePieceToBot ts piece }
react _ w = w


-- MAIN

main :: IO ()
main = play (InWindow "Tetris" (totalStageWidth, stageHeight) (200, 200))
            (greyN 1.1)
            2
            initialWorld
            render
            react
            step
              --where gameSpeed = w_pieceSpeed







-- CAN IGNORE: OLD FUNCTIONS AND DATA

{- if keyL == True && keyR == False
                                then if keyD == True
                                        then w { w_piece = translatePiece placedP (-1,-2) p }
                                    else w { w_piece = translatePiece placedP (-1,-1) p }
                               else if keyL == False && keyR == True
                                then if keyD == True
                                        then w { w_piece = translatePiece placedP (1,-2) p }
                                    else w { w_piece = translatePiece placedP (1,-1) p }
                               else if keyL == False && keyR == False && keyD == True
                                    then w { w_piece = translatePiece placedP (0,-2) p }
                               else w { w_piece = translatePiece placedP (0,-1) p } -}

-- renderPieceList :: [Piece] -> Picture
-- renderPieceList [] = blank
-- renderPieceList [p] = renderPiece p
-- renderPieceList ps = renderPiece (head ps) <> renderPieceList (tail ps)


{-restartWorld = World { w_playing = False
                     , w_piece = pick shapeList
                     , w_placedPieces = []
                     , w_nextPiece = pick shapeList
                     , w_startGame = False
                     , w_numPiecesP = 0
                     , w_prevGameNum = w_numPiecesP }
-}



    {- Rotate stuff
    False ->| (x > cen_x) && (y > cen_y)
                 = (x + tileS, y - (tileS*2))
            | (x > cen_x) && (y < cen_y)
                 = (x - (tileS*2), y + tileS)
            | (x < cen_x) && (y < cen_y)
                 = (x - tileS, y + (tileS*2))
            | (x < cen_x) && (y > cen_y)
                 = (x + (tileS*2), y - tileS)
            | otherwise = error "weird shape"
            -}


--bottomOut :: Piece -> [Float] -> Bool
--bottomOut p ls = bottomOutRec (nextPosStep p) ls

--bottomOutRec :: [Point] -> [Float] -> Bool
--bottomOutRec _ [] = False
--bottomOutRec ps@([(t0X,t0Y),(t1X,t1Y),(t2X,t2Y),(t3X,t3Y)] ) ls
--    | (t0Y < head ls || t1Y < head ls || t2Y < head ls || t3Y < head ls) 
--                = True
--    | otherwise = bottomOutRec ps (tail ls)

-- pathsOverlap :: Path -> Path -> Bool
-- pathsOverlap [a1,a2] [b1,b2]
--     = case intersectSegSeg a1 a2 b1 b2 of
--         Nothing -> False
--         Just (p) -> True

-- notOverlapPlacedPs :: [Piece] -> [Point] -> Bool
-- notOverlapPlacedPs [] _ = True
-- notOverlapPlacedPs ps pos 
--     | notOverlapPiece (head ps) pos = notOverlapPlacedPs (tail ps) pos
--     | otherwise = False