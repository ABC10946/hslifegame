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
        drawCell 0 0
      , color red $ drawCell fCellSize 0
      , color green $ drawCell 0 fCellSize
      , color blue $ drawCell fCellSize fCellSize   ]


drawCell :: Float -> Float -> Picture
drawCell x_pos y_pos = translate (x_base + x_pos) (y_base - y_pos) $ rectangleSolid fCellSize fCellSize
    where
        x_base = fromIntegral $ -(windowWidth `quot` 2) + (cellSize `quot` 2)
        y_base = fromIntegral $ (windowHeight `quot` 2) - (cellSize `quot` 2)


updateField :: Event -> FieldState -> FieldState
updateField _ = id

nextField :: Float -> FieldState -> FieldState
nextField _ = id

main :: IO ()
main = play window white 30 initialField drawField updateField nextField