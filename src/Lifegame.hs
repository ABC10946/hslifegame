module Lifegame
    ( 
        setCursorTop,
        initField,
        fieldChange,
        step,
        addSentinel,
        fieldFlip,
        checkCell,
        fieldCursorChange
    ) where

import Types

step :: Field -> Field
step field = [[lifeOrDead field (x,y) | x <- [0,1..(length (head field) -1)]]| y <- [0,1..(length field - 1)]]


lifeOrDead :: Field -> Position -> Cell
lifeOrDead field pos
    | countLifeInMooreNH field pos >= 4 = (False,Dead)
    | countLifeInMooreNH field pos == 3 = (False,Live)
    | countLifeInMooreNH field pos >= 2 = checkCell field pos
    | otherwise = (False,Dead)
    

checkCell :: Field -> Position -> Cell
checkCell field (x,y) = field !! y !! x

countLifeInMooreNH :: Field -> Position -> Int
countLifeInMooreNH field (x,y) = sum (map (cellToZeroOne field) [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)])

cellToZeroOne :: Field -> Position -> Int
cellToZeroOne field (x,y)
    |x >= 0 && y >= 0 && x < length (head field) && y < length field = boolToZeroOne (field !! y !! x)
    |otherwise = 0

boolToZeroOne :: Cell -> Int
boolToZeroOne (isCursor,cellState)
    |cellState == Live = 1
    |cellState == Dead = 0

initField :: ScreenSize -> Field
initField (width,height) = replicate height $ replicate width (False,Dead)

setCursorTop :: Field -> CursorState -> Field
setCursorTop (x:xs) isCursor = setCursorLeft x isCursor : xs

setCursorLeft :: Line -> CursorState -> Line
setCursorLeft (x:xs) isCursor = (isCursor,snd x):xs

fieldFlip :: Field -> Position -> Field
fieldFlip field pos 
    | snd (checkCell field pos) == Live = fieldChange Dead field pos
    | snd (checkCell field pos) == Dead = fieldChange Live field pos

fieldChange :: CState -> Field -> Position -> Field
fieldChange newValue (line:lines) (x,y)
    |y == 0 = newLine:lines
    |otherwise = line : fieldChange newValue lines (x,y-1)
    where
        newLine = lineChange x line newValue
        

lineChange :: LinePosition -> Line -> CState -> Line
lineChange linePos (x:xs) newValue
    |linePos == 0 = (fst x,newValue):xs
    |otherwise = x : lineChange (linePos-1) xs newValue

fieldCursorChange :: CursorState -> Field -> Position -> Field
fieldCursorChange newValue (line:lines) (x,y)
    |y == 0 = newLine:lines
    |otherwise = line : fieldCursorChange newValue lines (x,y-1)
    where
        newLine = lineCursorChange x line newValue

lineCursorChange :: LinePosition -> Line -> CursorState -> Line
lineCursorChange linePos (x:xs) newValue
    |linePos == 0 = (newValue,snd x):xs
    |otherwise = x : lineCursorChange (linePos-1) xs newValue

addSentinel :: Field -> Field
addSentinel field = field_
    where
        field_ = [cutHeadAndTail line|line <- cutHeadAndTail field]

cutHeadAndTail :: [a] -> [a]
cutHeadAndTail (x:xs) = init xs
