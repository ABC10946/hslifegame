module Main where

import Lifegame
import Draw
import Types
import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game


window :: Display
window = InWindow "Lifegame in Haskell!!" (windowWidth, windowHeight) (100,100)

initialFieldState :: FieldState
initialFieldState = FieldState 0 0 (setCursorTop (initField (fieldWidth,fieldHeight)) True)

up, left, down, right, enter :: FieldState -> FieldState
up    fieldS = move fieldS (cursor_x fieldS, cursor_y fieldS) CUp
left  fieldS = move fieldS (cursor_x fieldS, cursor_y fieldS) CLeft
down  fieldS = move fieldS (cursor_x fieldS, cursor_y fieldS) CDown
right fieldS = move fieldS (cursor_x fieldS, cursor_y fieldS) CRight
enter fieldS = fieldS {field = fieldFlip (field fieldS) (cursor_x fieldS,cursor_y fieldS)}

move :: FieldState -> Position -> CDirect -> FieldState
move fieldS (x,y) dir
    | dir == CUp    = fieldS {cursor_y = y_, field = fieldCursorChange True beforeMoveField p}
    | dir == CLeft  = fieldS {cursor_x = x_, field = fieldCursorChange True beforeMoveField p}
    | dir == CDown  = fieldS {cursor_y = y_, field = fieldCursorChange True beforeMoveField p}
    | dir == CRight = fieldS {cursor_x = x_, field = fieldCursorChange True beforeMoveField p}
    where
        beforeMoveField ::  Field
        beforeMoveField = fieldCursorChange False (field fieldS) (x,y)
        p@(x_,y_)
            | dir == CUp    = (x                           , (max (y-1) 0)                )
            | dir == CLeft  = ((max (x-1) 0)               , y                            )
            | dir == CDown  = (x                           , (min (y+1) (fieldHeight - 1)))
            | dir == CRight = ((min (x+1) (fieldWidth - 1)),y                             )

updateField :: Event -> FieldState -> FieldState
updateField (EventKey key ks _ _) fieldS = updateFieldWithKey key ks fieldS
updateField _                     fieldS = fieldS

updateFieldWithKey :: Key -> KeyState -> FieldState -> FieldState
updateFieldWithKey (SpecialKey KeyUp)    ks = if ks == Down then up    else id
updateFieldWithKey (SpecialKey KeyLeft)  ks = if ks == Down then left  else id
updateFieldWithKey (SpecialKey KeyDown)  ks = if ks == Down then down  else id
updateFieldWithKey (SpecialKey KeyRight) ks = if ks == Down then right else id
updateFieldWithKey (SpecialKey KeyEnter) ks = if ks == Down then enter else id
updateFieldWithKey _                     _  = id

nextField :: Float -> FieldState -> FieldState
nextField _ = id

main :: IO ()
main = play window white 30 initialFieldState drawField updateField nextField