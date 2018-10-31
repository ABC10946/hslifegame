module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

windowWidth, windowHeight :: Num a => a
windowWidth = 640
windowHeight = 480

window :: Display
window = InWindow "Lifegame in Haskell!!" (windowWidth, windowHeight) (100,100)

data FieldState = FieldState
    {
    }

initialField :: FieldState
initialField = undefined

drawField :: FieldState -> Picture
drawField = undefined

updateField :: Event -> FieldState -> FieldState
updateField = undefined

nextField :: Float -> FieldState -> FieldState
nextField = undefined

main :: IO ()
main = play window white 30 initialField drawField updateField nextField