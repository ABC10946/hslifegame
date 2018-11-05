module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

windowWidth, windowHeight :: Int
windowWidth = 640
windowHeight = 480

cellSize :: Int
cellSize = 20

fCellSize :: Float
fCellSize = fromIntegral cellSize

fieldWidth, fieldHeight :: Int
fieldWidth = windowWidth `quot` cellSize
fieldHeight = windowHeight `quot` cellSize

window :: Display
window = InWindow "Lifegame in Haskell!!" (windowWidth, windowHeight) (100,100)

data FieldState = FieldState
    {
        cursor_x :: Int
      , cursor_y :: Int
      , field :: [[Bool]]
    }

initialField :: FieldState
initialField = FieldState 0 0 (field fieldWidth fieldHeight)
    where
        field :: Int -> Int -> [[Bool]]
        field width height = replicate height $ replicate width False

drawField :: FieldState -> Picture
drawField fieldS = pictures [
        drawCell True 0 0
      , drawCell True 1 0
      , drawCell False 0 1
      , drawCell False 1 1
    ]


drawCell :: Bool -> Float -> Float -> Picture
drawCell isLive x_pos y_pos = translate (x_base + x_pos * fCellSize) (y_base - y_pos * fCellSize) $ rectPict fCellSize fCellSize
    where
        x_base = fromIntegral $ -(windowWidth `quot` 2) + (cellSize `quot` 2)
        y_base = fromIntegral $ (windowHeight `quot` 2) - (cellSize `quot` 2)
        rectPict = if isLive then rectangleSolid else rectangleWire


updateField :: Event -> FieldState -> FieldState
updateField _ = id

nextField :: Float -> FieldState -> FieldState
nextField _ = id

main :: IO ()
main = play window white 30 initialField drawField updateField nextField