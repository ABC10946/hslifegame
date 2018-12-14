module Types
    (
        FieldState(FieldState),
        ScreenSize,
        Field,
        Line,
        Position,
        LinePosition,
        Cell,
        CState(Live,Dead),
        CursorState,
        cursor_x,
        cursor_y,
        field,
        windowWidth,
        windowHeight,
        cellSize,
        fCellSize,
        fieldWidth,
        fieldHeight
    )
    where

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

data FieldState = FieldState
    {
        cursor_x :: Int
      , cursor_y :: Int
      , field :: Field
    }

type ScreenSize = (Int,Int)
type Field = [[Cell]]
type Line = [Cell]
type Position = (Int,Int)
type LinePosition = Int
type CursorState = Bool
type Cell = (CursorState,CState)
data CState = Live | Dead deriving Eq