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



up, left, down, right :: FieldState -> FieldState
up    fieldS = fieldS {cursor_y = cursor_y fieldS - 1, field = fieldCursorChange True (field fieldS) (cursor_x fieldS,cursor_y fieldS)}
left  fieldS = fieldS {cursor_x = cursor_x fieldS - 1, field = fieldCursorChange True (field fieldS) (cursor_x fieldS,cursor_y fieldS)}
down  fieldS = fieldS {cursor_y = cursor_y fieldS + 1, field = fieldCursorChange True (field fieldS) (cursor_x fieldS,cursor_y fieldS)}
right fieldS = fieldS {cursor_x = cursor_x fieldS + 1, field = fieldCursorChange True (field fieldS) (cursor_x fieldS,cursor_y fieldS)}



updateField :: Event -> FieldState -> FieldState
updateField (EventKey key ks _ _) fieldS = updateBoxWithKey key ks fieldS
updateField _                     fieldS = fieldS

updateBoxWithKey :: Key -> KeyState -> FieldState -> FieldState
updateBoxWithKey (SpecialKey KeyUp)    ks = if ks == Down then up else id
updateBoxWithKey (SpecialKey KeyLeft)  ks = if ks == Down then left else id
updateBoxWithKey (SpecialKey KeyDown)  ks = if ks == Down then down else id
updateBoxWithKey (SpecialKey KeyRight) ks = if ks == Down then right else id
updateBoxWithKey _                     _  = id

nextField :: Float -> FieldState -> FieldState
nextField _ = id

main :: IO ()
main = play window white 30 initialFieldState drawField updateField nextField