{-
Creator: Lake Giffen-Hunter
Email  : sgiffenh@haverford.edu

Sources:
tetris.wikia.com -- very helpful!!
-}

module Tetris where

import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid
import Data.Vector

stageWidth, stageHeight :: Int
stageWidth = 300
stageHeight = 500

stageWidthF, stageHeightF :: Float
stageWidthF = fromIntegral stageWidth
stageHeightF = fromIntegral stageHeight

pieceSpeed :: Float
pieceSpeed = 1

data Piece = Piece { color :: Color
                   , rotation :: 
                   


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
