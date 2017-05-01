

module GameData where


import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Line

totalStageWidth, stageWidth, stageHeight :: Int
stageWidth = 300
stageHeight = 540
-- TODO: make these scalable from tileS, then incorporate this into initial piece pos

totalStageWidth = stageHeight + 50

stageWidthF, stageHeightF :: Float
stageWidthF = fromIntegral stageWidth
stageHeightF = fromIntegral stageHeight

numPiecesPerLevel :: Int
numPiecesPerLevel = 20
-- not used yet, want to speed up simulation

leftWall :: Path
leftWall = [(0,0),(0,stageHeightF)]

rightWall :: Path
rightWall = [(stageWidthF,0),(stageWidthF,stageHeightF)]