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
drawField fieldS = pictures (map (drawCell fieldS) posMat)
    where
        posMat = [(x,y) | x <- [0,1..(length ((field fieldS) !! 0)-1)], y <- [0,1..((length (field fieldS))-1)]]


drawCell :: FieldState -> (Int,Int) -> Picture
drawCell fieldS (x_pos,y_pos) = translate (x_base + fX_pos * fCellSize) (y_base - fY_pos * fCellSize) $ rectPict
    where
        x_base = fromIntegral $ -(windowWidth `quot` 2) + (cellSize `quot` 2)
        y_base = fromIntegral $ (windowHeight `quot` 2) - (cellSize `quot` 2)
        cState = checkCell (field fieldS) (x_pos,y_pos)

        rectPict 
            |cState == (True,Live) = color magenta $ rectangleSolid (fCellSize+2) (fCellSize+2)
            |fst cState == True    = color green $ rectangleSolid (fCellSize+2) (fCellSize+2)
            |snd cState == Live    = rectangleSolid fCellSize fCellSize
            |snd cState == Dead    = rectangleWire fCellSize fCellSize
            |otherwise = blank

        fX_pos = fromIntegral x_pos
        fY_pos = fromIntegral y_pos
