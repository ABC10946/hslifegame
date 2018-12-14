module Draw
    (
        drawField,
        drawCell
    )
    where

import Graphics.Gloss
import Types
import Lifegame

drawField :: FieldState -> Picture
drawField fieldS = pictures $ [pictures (map (drawCell fieldS) posMat), text $ show (cursor_x fieldS, cursor_y fieldS)]
    where
        posMat = [(x,y) | x <- [0,1..(length ((field fieldS) !! 0)-1)], y <- [0,1..((length (field fieldS))-1)]]


drawCell :: FieldState -> (Int,Int) -> Picture
drawCell fieldS (x_pos,y_pos) = translate (x_base + fX_pos * fCellSize) (y_base - fY_pos * fCellSize) $ rectPict
    where
        x_base = fromIntegral $ -(windowWidth `quot` 2) + (cellSize `quot` 2)
        y_base = fromIntegral $ (windowHeight `quot` 2) - (cellSize `quot` 2)
        cState = checkCell (field fieldS) (x_pos,y_pos)

        rectPict |snd cState == Live = rectangleSolid fCellSize fCellSize
                 |snd cState == Dead = rectangleWire fCellSize fCellSize
                 |fst cState == True = color black $ rectangleSolid (fCellSize+2) (fCellSize+2)
                 |otherwise = blank

        fX_pos = fromIntegral x_pos
        fY_pos = fromIntegral y_pos
