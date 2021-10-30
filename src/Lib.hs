{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Lib
    ( runGame
    ) where

import Control.Monad
import Data.IORef
import Data.Time.Clock
import Data.List.Safe as Safe
import Data.List.Index (indexed)
import Data.Maybe (fromMaybe)
import FRP.Yampa as Yampa
import SDL as SDL
import SDL.Vect
import Debug.Trace

data GameState = GameState
  { finished :: Bool
  , currentBlock :: PlacedBlock
  , playFieldState :: PlayFieldState
  }

initialGameState :: GameState
initialGameState =
  GameState False defaultBlock initialPlayFieldState

data ButtonPresses = ButtonPresses
  { upArrow :: Bool
  , downArrow :: Bool
  , leftArrow :: Bool
  , rightArrow :: Bool
  , rotateL :: Bool
  , rotateR :: Bool
  , hardDrop :: Bool
  , quitKey :: Bool
  }
  deriving (Show, Eq)

noButtonPressed :: ButtonPresses
noButtonPressed = ButtonPresses False False False False False False False False

data GameScreen = GameScreen
  { playFieldLeft :: Int
  , playFieldTop :: Int
  }

data PlacedBlock = PlacedBlock
  { blockShape :: BlockShape
  , orientation :: BlockOrientation
  , position :: BlockPosition
  }
  deriving (Show, Eq)

data BlockShape
  = O
  | I
  deriving (Show, Eq)

data BlockOrientation
  = Deg0
  | Deg90
  | Deg180
  | Deg270
  deriving (Show, Eq)

data BlockPosition
  = Position Int Float
  | NoPosition
  deriving (Show, Eq)

type PlayFieldState = [[Bool]]

-- play field is 20 x 10. There are extra rows at the top (think row -1 and -2)
initialPlayFieldState :: PlayFieldState
initialPlayFieldState = replicate 22 blankRow

blankRow :: [Bool]
blankRow = replicate 10 False

runGame :: RandomGen rg => rg -> IO ()
runGame rg = do
  renderer <- initScreen
  t <- getCurrentTime
  timeRef <- newIORef (t, t)
  reactimate initialise (input timeRef) (output renderer) (process rg initialGameState)

initialise :: IO [SDL.Event]
initialise = do
  putStrLn "Starting..."
  return []

input :: IORef (UTCTime, UTCTime) -> Bool -> IO (DTime, Maybe [SDL.Event])
input ref _ = do
  currTime <- getCurrentTime
  (start, lastTime) <- readIORef ref
  writeIORef ref (start, currTime)
  events <- pollEvents
  let dt = diffUTCTime currTime lastTime
  return (realToFrac dt, Just events)

buttonPressesFrom :: [SDL.Event] -> ButtonPresses
buttonPressesFrom events =
  let u = any (checkKeyPress [ScancodeUp]) events
      d = any (checkKeyPress [ScancodeDown]) events
      l = any (checkKeyPress [ScancodeLeft]) events
      r = any (checkKeyPress [ScancodeRight]) events
      shift = any (checkKeyPress [ScancodeLShift, ScancodeRShift]) events
      ctrl = any (checkKeyPress [ScancodeLCtrl, ScancodeRCtrl]) events
      sp = any (checkKeyPress [ScancodeSpace]) events
      q = any (checkKeyPress [ScancodeEscape]) events
  in
  ButtonPresses u d l r shift ctrl sp q

checkKeyPress :: [Scancode] -> SDL.Event -> Bool
checkKeyPress scanCodes event =
  case eventPayload event of
    KeyboardEvent keData ->
      isPress keData && any (\code -> scancodeOf keData == code) scanCodes
    _ ->
      False

isPress :: KeyboardEventData -> Bool
isPress ked =
  keyboardEventKeyMotion ked == Pressed

scancodeOf :: KeyboardEventData -> Scancode
scancodeOf ked =
  keysymScancode $ keyboardEventKeysym ked

output :: Renderer -> Bool -> GameState -> IO Bool
output renderer _ gs = do
  events <- pollEvents
  rendererDrawColor renderer $= V4 32 32 32 255
  clear renderer
  drawPlayField (playFieldState gs) renderer
  drawBlock (currentBlock gs) renderer
  -- drawSquare (position $ currentBlock gs) renderer
  drawBorders renderer
  present renderer
  currTime <- getCurrentTime
  when (finished gs) (putStrLn "Done")
  return (finished gs)

drawBlock :: PlacedBlock -> Renderer -> IO ()
drawBlock PlacedBlock { position = NoPosition } _ = return ()
drawBlock PlacedBlock { blockShape = shape, orientation = orientation, position = Position x y } renderer =
  let
    blockMap = get4x4 shape orientation
    renderRow position (idy, row) = sequence $ map (renderCell (fromIntegral idy)) (indexed row)
    renderCell idy (idx, blockExists) = when blockExists (drawSquare (Position (x + idx) (y + idy)) renderer)
  in
  sequence_ $ map (renderRow position) (indexed  blockMap)

drawSquare :: BlockPosition -> Renderer -> IO ()
drawSquare NoPosition _ = return ()
drawSquare (Position x y) renderer = do
  rendererDrawColor renderer $= V4 192 32 32 255
  fillRect renderer (Just (Rectangle (P (V2 (fromIntegral (x*20+40)) (fromIntegral ((floor y)*20)))) (V2 20 20)))


drawPlayField :: PlayFieldState -> Renderer -> IO ()
drawPlayField playField renderer = do
  sequence_ $
    map
      (\(idy, row) ->
        sequence $
          map
            (\(idx, cell) ->
              when cell (drawSquare (Position idx (fromIntegral idy)) renderer)
            )
            (indexed row)
      )
      (indexed playField)

drawBorders :: Renderer -> IO ()
drawBorders renderer =
  let playFieldRect = Rectangle (P (V2 40 40)) (V2 200 400)
  in
  drawRect renderer (Just playFieldRect)

process :: RandomGen rg => rg -> GameState -> SF [SDL.Event] GameState
process rg initialState =
    (Yampa.identity &&& Yampa.constant Running) >>> (setBlockPosition rg initialState)

buildGameState :: GameState -> PlacedBlock -> GameState
buildGameState initialState block =
  initialState { currentBlock = block }

data GameEvent
  = Running
  | Quitting
  | Deleting [Int] PlacedBlock
  | Landing [BlockPosition]
  | BlockMove PlacedBlock
  deriving (Show, Eq)

setY :: (BlockPosition, Float) -> BlockPosition
setY (NoPosition, _) = NoPosition
setY ((Position x y), dy) = Position x (y + dy)

setBlockPosition :: RandomGen rg => rg -> GameState -> SF ([SDL.Event], GameEvent) GameState
setBlockPosition rg gs = switch (sf >>> second notYet) cont
  where sf = proc (events, gm) -> do
          let buttonPresses = buttonPressesFrom events
              block = currentBlock gs
              pos = position $ block
              (nextBlock, nextRg) = randomBlock rg
          dy <- integral -< (1.0 :: Float)
          newPosition <- arr setY -< (pos, dy)
          gameModeEvent <- arr computeGameMode -< (buttonPresses, (playFieldState gs), nextBlock, pos)
          landingEvent <- arr landBlock -< (block { position = newPosition }, (playFieldState gs))
          newGameState <- arr (buildGameState gs) -< block { position = newPosition }
          moveEvent <- arr moveBlock -< (nextBlock, buttonPresses, block { position = newPosition }, (playFieldState gs))
          returnA -< (newGameState, (gameModeEvent `lMerge` landingEvent `lMerge` moveEvent) `attach` nextRg)
        cont (Quitting, rg) =
          setBlockPosition rg (gs { finished = True })
        cont (Deleting indexes nextBlock, rg) =
          let
            playField = playFieldState gs
            updatedPlayField = (replicate (length indexes) blankRow) ++ (foldr removeRow playField indexes)
            updatedGameState = gs { playFieldState = updatedPlayField, currentBlock = nextBlock }
          in
          pause (foldr replaceWithBlankRow  gs indexes) (Yampa.localTime >>^ (< 1.0)) (setBlockPosition rg updatedGameState)
        cont (Running, rg) =
          setBlockPosition rg gs
        cont (Landing positions, rg) =
          let
            block = currentBlock gs
            gsWithHiddenBlock =
              gs { currentBlock = block { position = NoPosition } }
          in
          pause gs (Yampa.localTime >>^ (< 1.0)) (setBlockPosition rg (foldr placeSquare gsWithHiddenBlock positions))
        cont (BlockMove placedBlock, rg) =
          setBlockPosition rg (gs { currentBlock = placedBlock })

computeGameMode :: (ButtonPresses, PlayFieldState, BlockShape, BlockPosition) -> Yampa.Event GameEvent
computeGameMode (bp, field, nextBlock, position) =
  let
    fullRows =
      filter (\(idx, _) -> idx > 1 && idx < 22) $ filter isFullRow (indexed field)
    isFullRow (_, list) = all id (slice 1 10 list)
  in
  if quitKey bp then
    Yampa.Event Quitting
  else if fullRows /= [] then
    Yampa.Event (Deleting (map fst fullRows) (PlacedBlock nextBlock Deg0 (Position 3 2)))
  else if position == NoPosition then
    Yampa.Event (BlockMove (PlacedBlock nextBlock Deg0 (Position 3 2)))
  else
    Yampa.NoEvent

placeSquare :: BlockPosition -> GameState -> GameState
placeSquare NoPosition gs = gs
placeSquare (Position x y) gs =
  let
    intY = floor y
    playField = playFieldState gs
    -- TODO: introduce lens for this kind of stuff?
    row = fromMaybe [] $ Safe.head $ slice intY intY playField
    (beforeX, xAndAfter) = splitAt x row
    newRow = beforeX ++ [True] ++ (fromMaybe [] $ Safe.tail xAndAfter)
    (beforeY, yAndAfter) = splitAt intY playField
    newPlayField = beforeY ++ [newRow] ++ (fromMaybe [] $ Safe.tail yAndAfter)
  in
    gs { playFieldState = newPlayField }

replaceWithBlankRow :: Int -> GameState -> GameState
replaceWithBlankRow idx gs =
  let
    playField = playFieldState gs
    (beforeElem, elemAndAfter) = splitAt idx playField
    newPlayField = beforeElem ++ [blankRow] ++ (fromMaybe [] $ Safe.tail elemAndAfter)
  in
  gs { playFieldState = newPlayField }

trc :: Show a => String -> a -> a
trc str a = Debug.Trace.trace (str ++ ": " ++ (show a)) a

removeRow :: Int -> PlayFieldState -> PlayFieldState
removeRow idx playField =
  let
    (beforeElem, elemAndAfter) = splitAt idx playField
  in
  beforeElem ++ (fromMaybe [] $ Safe.tail elemAndAfter)

-- slices list from index to another (both inclusive)
slice :: Int -> Int -> [a] -> [a]
slice from to xs =
  if from > 0 then
    take (to - from + 1) (drop from xs)
  else
    take (to + 1) xs

landBlock :: (PlacedBlock, PlayFieldState) -> Yampa.Event GameEvent
landBlock (PlacedBlock { position = NoPosition }, playFieldState) = Yampa.NoEvent
landBlock (block@(PlacedBlock { position = Position x y }), playFieldState) =
  let
    blockStopped = not $ canMoveTo block playFieldState
    landingBlocks = blocksToKeep blockStopped block
  in
  if blockStopped then
    Yampa.Event (Landing landingBlocks)
  else
    Yampa.NoEvent

moveBlock :: (BlockShape, ButtonPresses, PlacedBlock, PlayFieldState) -> Yampa.Event GameEvent
moveBlock (_, _, PlacedBlock { position = NoPosition }, _) = Yampa.NoEvent
moveBlock (nextBlockShape, buttons, block@(PlacedBlock { position = (Position x y), blockShape = shape, orientation = orientation }), playFieldState) =
  let
    canMoveDown = canMoveTo (block { position = (Position x (y + 1)) }) playFieldState
  in
  if (rotateL buttons) && (canRotateL block playFieldState) then
    Yampa.Event $ BlockMove (block { orientation = spinLeft orientation })
  else if (rotateR buttons) && (canRotateR block playFieldState) then
    Yampa.Event $ BlockMove (block { orientation = spinRight orientation })
  else if (leftArrow buttons) && (canMoveLeft block playFieldState) then
    Yampa.Event $ BlockMove (block { position = (Position (x - 1) y) })
  else if (rightArrow buttons) && (canMoveRight block playFieldState) then
    Yampa.Event $ BlockMove (block { position = (Position (x + 1) y) })
  else if downArrow buttons && canMoveDown then
    Yampa.Event $ BlockMove (block { position = (Position x (y + 1)) })
  else if not canMoveDown then
    Yampa.Event $ BlockMove block
  else
    Yampa.NoEvent

-- TODO: Stop hard coding range... count enum?
randomBlock :: RandomGen rg => rg -> (BlockShape, rg)
randomBlock rg =
  let
    (i, newRg) = randomR (0::Int, 1::Int) rg
  in
  if i == 0 then
    (I, newRg)
  else
    (O, newRg)

blocksToKeep :: Bool -> PlacedBlock -> [BlockPosition]
blocksToKeep blockStopped (PlacedBlock { position = NoPosition }) = []
blocksToKeep blockStopped block@(PlacedBlock { position = Position x overlappingY, blockShape = shape, orientation = orientation }) =
  let
    y = overlappingY - 1
    blockMap = get4x4 shape orientation
  in
  if blockStopped then
    concatMap (\(idy, row) ->
      map (\(idx, _) ->
        Position (x + idx) (y + (fromIntegral idy))
      ) (filter (\(_, v) -> v)
        (indexed row)
      )
    ) (indexed blockMap)
  else
    []

canMoveTo :: PlacedBlock -> PlayFieldState -> Bool
canMoveTo (PlacedBlock { position = NoPosition }) playFieldState = False
canMoveTo (block@(PlacedBlock { position = Position x y, blockShape = shape, orientation = orientation })) playFieldState =
  let
    intY = floor y
    rows = slice intY (intY + 3) (playFieldState ++ [(replicate 10 True)]) -- adding the floor (10 Trues)
    -- Trues are walls, Falses needed to allow 'I' block to go to the edges
    augmentedRows = map (\row -> [False, True] ++ row ++ [True, False]) rows
    cells = concatMap (slice (x + 2) (x + 5)) augmentedRows
    blockCells = concat $ get4x4 shape orientation
  in
  not $ any (\(l, r) -> l && r) (zip (trc "cells" cells) (trc "blockCells" blockCells))

canMoveLeft :: PlacedBlock -> PlayFieldState -> Bool
canMoveLeft (PlacedBlock { position = NoPosition }) playFieldState = False
canMoveLeft (block@(PlacedBlock { position = Position x y })) playFieldState =
  canMoveTo (block { position = Position (x - 1) y }) playFieldState

canMoveRight :: PlacedBlock -> PlayFieldState -> Bool
canMoveRight (PlacedBlock { position = NoPosition }) playFieldState = False
canMoveRight (block@(PlacedBlock { position = Position x y })) playFieldState =
  canMoveTo (block { position = Position (x + 1) y }) playFieldState

canRotateL :: PlacedBlock -> PlayFieldState -> Bool
canRotateL (block@(PlacedBlock { orientation = orientation })) playFieldState =
  canMoveTo (block { orientation = spinLeft orientation }) playFieldState

canRotateR :: PlacedBlock -> PlayFieldState -> Bool
canRotateR (block@(PlacedBlock { orientation = orientation })) playFieldState =
  canMoveTo (block { orientation = spinRight orientation }) playFieldState


spinLeft :: BlockOrientation -> BlockOrientation
spinLeft Deg0 = Deg90
spinLeft Deg90 = Deg180
spinLeft Deg180 = Deg270
spinLeft Deg270 = Deg0

spinRight :: BlockOrientation -> BlockOrientation
spinRight Deg0 = Deg270
spinRight Deg90 = Deg0
spinRight Deg180 = Deg90
spinRight Deg270 = Deg180

defaultBlock :: PlacedBlock
defaultBlock =
  PlacedBlock
    { blockShape = O
    , orientation = Deg0
    , position = Position 2 2
    }

initScreen :: IO Renderer
initScreen = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  return renderer


get4x4 :: BlockShape -> BlockOrientation -> [[Bool]]
get4x4 O _ =
  [ [False, True, True, False]
  , [False, True, True, False]
  , [False, False, False, False]
  , [False, False, False, False]
  ]
get4x4 I Deg0 =
  [ [False, True, False, False]
  , [False, True, False, False]
  , [False, True, False, False]
  , [False, True, False, False]
  ]
get4x4 I Deg90 =
  [ [False, False, False, False]
  , [False, False, False, False]
  , [True, True, True, True]
  , [False, False, False, False]
  ]
get4x4 I Deg180 =
  [ [False, False, True, False]
  , [False, False, True, False]
  , [False, False, True, False]
  , [False, False, True, False]
  ]
get4x4 I Deg270 =
  [ [False, False, False, False]
  , [True, True, True, True]
  , [False, False, False, False]
  , [False, False, False, False]
  ]

