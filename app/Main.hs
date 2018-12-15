module Main where

import Lifegame
import Draw
import Types
import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game
import Data.Fixed
import Debug.Trace


window :: Display
window = InWindow "Lifegame in Haskell!!" (windowWidth, windowHeight) (100,100)

initialFieldState :: FieldState
initialFieldState = FieldState 0 0 (setCursorTop (initField (fieldWidth,fieldHeight)) True) False

up, left, down, right, enter, space, leftButton :: FieldState -> FieldState
up    fieldS = move fieldS (cursor_x fieldS, cursor_y fieldS) CUp
left  fieldS = move fieldS (cursor_x fieldS, cursor_y fieldS) CLeft
down  fieldS = move fieldS (cursor_x fieldS, cursor_y fieldS) CDown
right fieldS = move fieldS (cursor_x fieldS, cursor_y fieldS) CRight
enter fieldS = fieldS {field  = fieldFlip (field fieldS) (cursor_x fieldS,cursor_y fieldS)}
space fieldS = fieldS {isPlay = not (isPlay fieldS)}
leftButton = enter

move :: FieldState -> Position -> CDirect -> FieldState
move fieldS (x,y) dir
    | dir == CUp    = fieldS {cursor_y = y_, field = fieldCursorChange True beforeMoveField p}
    | dir == CLeft  = fieldS {cursor_x = x_, field = fieldCursorChange True beforeMoveField p}
    | dir == CDown  = fieldS {cursor_y = y_, field = fieldCursorChange True beforeMoveField p}
    | dir == CRight = fieldS {cursor_x = x_, field = fieldCursorChange True beforeMoveField p}
    where
        beforeMoveField ::  Field
        beforeMoveField = clearCursorOnField (field fieldS)
        p@(x_,y_)
            | dir == CUp    = (x                           , max (y-1) 0                  )
            | dir == CLeft  = (max (x-1) 0                 , y                            )
            | dir == CDown  = (x                           , min (y+1) (fieldHeight - 1)  )
            | dir == CRight = (min (x+1) (fieldWidth - 1)  ,y                             )

updateField :: Event -> FieldState -> FieldState
updateField (EventKey key ks _ _) fieldS = updateFieldWithKey key ks fieldS
updateField (EventMotion (x, y))  fieldS = updateFieldWithMouseMotion fieldS x y
updateField _                     fieldS = fieldS

updateFieldWithMouseMotion :: FieldState -> Float -> Float -> FieldState
updateFieldWithMouseMotion fieldS mx my = fieldS {cursor_x = mxF, cursor_y = myF, field = fieldCursorChange True beforeMoveField (mxF,myF)}
    where
        fWinWidth, fWinHeight :: Float
        fWinWidth  = fromIntegral windowWidth
        fWinHeight = fromIntegral windowHeight

        mx_,my_ :: Float
        mx_ = mx + (fWinWidth / 2)
        my_ = (-my) + (fWinHeight / 2)
        mxF = (mx_ `div'` fCellSize) :: Int
        myF = (my_ `div'` fCellSize) :: Int
        beforeMoveField = clearCursorOnField (field fieldS)

clearCursorOnField :: Field -> Field
clearCursorOnField = map clearCursorOnLine 

clearCursorOnLine :: Line -> Line
clearCursorOnLine = map clearCursor

clearCursor :: Cell -> Cell
clearCursor (_, cState) = (False, cState)

updateFieldWithKey :: Key -> KeyState -> FieldState -> FieldState
updateFieldWithKey (SpecialKey KeyUp)       ks = if ks == Down then up         else id
updateFieldWithKey (SpecialKey KeyLeft)     ks = if ks == Down then left       else id
updateFieldWithKey (SpecialKey KeyDown)     ks = if ks == Down then down       else id
updateFieldWithKey (SpecialKey KeyRight)    ks = if ks == Down then right      else id
updateFieldWithKey (SpecialKey KeyEnter)    ks = if ks == Down then enter      else id
updateFieldWithKey (SpecialKey KeySpace)    ks = if ks == Down then space      else id
updateFieldWithKey (MouseButton LeftButton) ks = if ks == Down then leftButton else id
updateFieldWithKey _                     _  = id

nextField :: Float -> FieldState -> FieldState
nextField _ fieldS
    | isPlay fieldS   = fieldS {field = step (field fieldS)}
    | otherwise       = fieldS

main :: IO ()
main = play window white 15 initialFieldState drawField updateField nextField