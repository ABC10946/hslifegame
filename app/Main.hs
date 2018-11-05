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
drawField fieldS = pictures $ map (drawCell fieldS) posMat
    where
        posMat = [(x,y) | x <- [0,1..(length ((field fieldS) !! 0)-1)], y <- [0,1..((length (field fieldS))-1)]]


drawCell :: FieldState -> (Int,Int) -> Picture
drawCell fieldS (x_pos,y_pos) = translate (x_base + fX_pos * fCellSize) (y_base - fY_pos * fCellSize) $ rectPict fCellSize fCellSize
    where
        x_base = fromIntegral $ -(windowWidth `quot` 2) + (cellSize `quot` 2)
        y_base = fromIntegral $ (windowHeight `quot` 2) - (cellSize `quot` 2)
        rectPict = if isLive then rectangleSolid else rectangleWire
        isLive = (((field fieldS) !! y_pos) !! x_pos) 
        fX_pos = fromIntegral x_pos
        fY_pos = fromIntegral y_pos


updateField :: Event -> FieldState -> FieldState
updateField _ = id

nextField :: Float -> FieldState -> FieldState
nextField _ = id

main :: IO ()
main = play window white 30 initialField drawField updateField nextField