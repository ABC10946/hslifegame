module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

windowWidth, windowHeight :: Int
windowWidth = 640
windowHeight = 480

cellSize :: Int
cellSize = 20

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
        translate x_pos y_pos $ rectangleSolid fCellSize fCellSize
      , color red $ translate (x_pos + fCellSize) y_pos $ rectangleSolid fCellSize fCellSize
      , color green $ translate x_pos (y_pos - fCellSize) $ rectangleSolid fCellSize fCellSize
      , color blue $ translate (x_pos + fCellSize) (y_pos - fCellSize) $ rectangleSolid fCellSize fCellSize

    ]
    where
        x_pos = fromIntegral $ -(windowWidth `quot` 2) +  (cellSize `quot` 2)
        y_pos = fromIntegral $ (windowHeight `quot` 2) - (cellSize `quot` 2)
        fCellSize = fromIntegral cellSize

updateField :: Event -> FieldState -> FieldState
updateField _ = id

nextField :: Float -> FieldState -> FieldState
nextField _ = id

main :: IO ()
main = play window white 30 initialField drawField updateField nextField