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
import Data.Vector.Storable (fromList)
import Debug.Trace

data GameState = GameState
  { finished :: Bool
  , currentBlock :: PlacedBlock
  , nextBlockShape :: BlockShape
  , playFieldState :: PlayFieldState
  , score :: Int
  , speed :: Float
  }

initialGameState :: BlockShape -> BlockShape -> GameState
initialGameState shape nextShape =
  let
    initialBlock = PlacedBlock shape Deg0 (Position 3 2) False
  in
  GameState False initialBlock nextShape initialPlayFieldState 0 1.0

data ButtonPresses = ButtonPresses
  { downArrow :: Bool
  , leftArrow :: Bool
  , rightArrow :: Bool
  , rotateL :: Bool
  , rotateR :: Bool
  , hardDrop :: Bool
  , quitKey :: Bool
  }
  deriving (Show, Eq)

noButtonPressed :: ButtonPresses
noButtonPressed = ButtonPresses False False False False False False False

data GameScreen = GameScreen
  { playFieldLeft :: Int
  , playFieldTop :: Int
  }

data PlacedBlock = PlacedBlock
  { blockShape :: BlockShape
  , orientation :: BlockOrientation
  , position :: BlockPosition
  , flashing :: Bool
  }
  deriving (Show, Eq)

data BlockShape
  = O
  | I
  | J
  | L
  | S
  | T
  | Z
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

data BlockColour
  = Red
  | Cyan
  | Blue
  | Orange
  | Yellow
  | Green
  | Purple
  | White
  | Grey -- game over only
  | NoBlock
  deriving (Show, Eq)

type PlayFieldState = [[BlockColour]]
type PlayFieldBoolMap = [[Bool]]

colourOf :: PlacedBlock -> BlockColour
colourOf PlacedBlock { flashing = True } = White
colourOf PlacedBlock { blockShape = I } = Cyan
colourOf PlacedBlock { blockShape = J } = Blue
colourOf PlacedBlock { blockShape = L } = Orange
colourOf PlacedBlock { blockShape = O } = Yellow
colourOf PlacedBlock { blockShape = S } = Green
colourOf PlacedBlock { blockShape = T } = Purple
colourOf PlacedBlock { blockShape = Z } = Red

-- play field is 20 x 10. There are extra rows at the top (think row -1 and -2)
initialPlayFieldState :: PlayFieldState
initialPlayFieldState = replicate 22 blankRow

gameOverFieldState :: PlayFieldState
gameOverFieldState = (replicate 2 $ replicate 10 NoBlock) ++ (replicate 20 $ replicate 10 Grey)

blankRow :: [BlockColour]
blankRow = replicate 10 NoBlock

toBoolMap :: PlayFieldState -> PlayFieldBoolMap
toBoolMap playField =
  map (\row -> map (\cell -> cell /= NoBlock) row) playField

runGame :: RandomGen rg => rg -> IO ()
runGame rg = do
  let
    (blockShape, rg') = randomBlock rg
    (nextBlockShape, newRg) = randomBlock rg'
  renderer <- initScreen
  t <- getCurrentTime
  timeRef <- newIORef (t, t)
  reactimate initialise (input timeRef) (output renderer) (process newRg (initialGameState blockShape nextBlockShape))

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
  let
      d = any (checkKeyPress [ScancodeDown]) events
      l = any (checkKeyPress [ScancodeLeft]) events
      r = any (checkKeyPress [ScancodeRight]) events
      shift = any (checkKeyPress [ScancodeLShift, ScancodeRShift]) events
      ctrl = any (checkKeyPress [ScancodeLCtrl, ScancodeRCtrl]) events
      sp = any (checkKeyPress [ScancodeSpace, ScancodeUp]) events
      q = any (checkKeyPress [ScancodeEscape]) events
  in
  ButtonPresses d l r shift ctrl sp q

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
  rendererDrawColor renderer $= V4 8 8 8 255
  clear renderer
  drawPlayField (playFieldState gs) renderer
  drawBlock (currentBlock gs) renderer
  drawBorders renderer
  renderScore ((score gs) * 100) renderer
  renderNextBlock (nextBlockShape gs) renderer
  present renderer
  currTime <- getCurrentTime
  when (finished gs) (putStrLn "Done")
  return (finished gs)

renderScore :: Int -> Renderer -> IO ()
renderScore score renderer =
  let
    digits = show score
    len = length digits
    start = (14 - len)
  in
  do
    rendererDrawColor renderer $= V4 224 224 224 255
    sequence_ $
      map (drawDigit start renderer) (indexed digits)

renderNextBlock :: BlockShape -> Renderer -> IO ()
renderNextBlock shape renderer =
  drawBlock (PlacedBlock { blockShape = shape, orientation = Deg0, position = Position 14 4, flashing = False }) renderer

drawBlock :: PlacedBlock -> Renderer -> IO ()
drawBlock PlacedBlock { position = NoPosition } _ = return ()
drawBlock block@(PlacedBlock { blockShape = shape, orientation = orientation, position = Position x y }) renderer =
  let
    blockMap = get4x4 shape orientation
    renderRow position (idy, row) = sequence $ map (renderCell (fromIntegral idy)) (indexed row)
    renderCell idy (idx, blockExists) = when blockExists (drawSquare (colourOf block) (Position (x + idx) (y + idy)) renderer)
  in
  sequence_ $ map (renderRow position) (indexed  blockMap)

normalColour :: Num a => BlockColour -> V4 a
normalColour Red = V4 192 32 32 255
normalColour Cyan = V4 32 160 192 255
normalColour Blue = V4 32 32 192 255
normalColour Orange = V4 192 128 32 255
normalColour Yellow = V4 192 192 32 255
normalColour Green = V4 32 192 32 255
normalColour Purple = V4 128 32 128 255
normalColour Grey = V4 128 128 128 255
normalColour White = V4 192 192 192 255

lightColour :: Num a => BlockColour -> V4 a
lightColour Red = V4 224 64 64 255
lightColour Cyan = V4 128 192 224 255
lightColour Blue = V4 64 64 224 255
lightColour Orange = V4 224 160 64 255
lightColour Yellow = V4 224 224 96 255
lightColour Green = V4 64 224 64 255
lightColour Purple = V4 160 64 160 255
lightColour Grey = V4 160 160 160 255
lightColour White = V4 224 224 224 255

lightestColour :: Num a => BlockColour -> V4 a
lightestColour Red = V4 224 96 96 255
lightestColour Cyan = V4 192 224 255 255
lightestColour Blue = V4 96 96 224 255
lightestColour Orange = V4 224 192 96 255
lightestColour Yellow = V4 224 224 128 255
lightestColour Green = V4 96 224 96 255
lightestColour Purple = V4 224 96 224 255
lightestColour Grey = V4 224 224 224 255
lightestColour White = V4 255 255 255 255

darkColour :: Num a => BlockColour -> V4 a
darkColour Red = V4 128 32 32 255
darkColour Cyan = V4 32 64 128 255
darkColour Blue = V4 32 32 128 255
darkColour Orange = V4 128 64 32 255
darkColour Yellow = V4 128 128 32 255
darkColour Green = V4 32 128 32 255
darkColour Purple = V4 64 32 64 255
darkColour Grey = V4 32 32 32 255
darkColour White = V4 160 160 160 255

drawSquare :: BlockColour ->  BlockPosition -> Renderer -> IO ()
drawSquare _ NoPosition _ = return ()
drawSquare colour (Position x y) renderer = do
  let
    leftX = x * 20 + 40
    topY = (floor y) * 20
  rendererDrawColor renderer $= normalColour colour
  fillRect renderer (Just (Rectangle (P (V2 (fI leftX) (fI topY))) (V2 20 20)))
  rendererDrawColor renderer $= lightColour colour
  drawLine renderer (P (V2 (fI leftX) (fI topY))) (P (V2 (fI (leftX + 19)) (fI topY)))
  drawLine renderer (P (V2 (fI leftX) (fI topY))) (P (V2 (fI leftX) (fI (topY + 19))))
  rendererDrawColor renderer $= darkColour colour
  drawLine renderer (P (V2 (fI (leftX + 1)) (fI (topY + 19)))) (P (V2 (fI (leftX + 19)) (fI (topY + 19))))
  drawLine renderer (P (V2 (fI (leftX + 19)) (fI (topY + 1)))) (P (V2 (fI (leftX + 19)) (fI (topY + 19))))
  rendererDrawColor renderer $= lightestColour colour
  drawPoint renderer (P (V2 (fI leftX) (fI topY)))


drawPlayField :: PlayFieldState -> Renderer -> IO ()
drawPlayField playField renderer = do
  sequence_ $
    map
      (\(idy, row) ->
        sequence $
          map
            (\(idx, cell) ->
              when (cell /= NoBlock) (drawSquare cell (Position idx (fromIntegral idy)) renderer)
            )
            (indexed row)
      )
      (indexed playField)

drawBorders :: Renderer -> IO ()
drawBorders renderer = do
  let playFieldRect = Rectangle (P (V2 39 39)) (V2 201 401)
  rendererDrawColor renderer $= V4 224 224 224 255
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
  | Deleting [Int]
  | Landing [BlockPosition]
  | BlockMove PlacedBlock
  | GameOver
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
          dy <- integral -< speed gs
          newPosition <- arr setY -< (pos, dy)
          gameModeEvent <- arr computeGameMode -< (buttonPresses, (toBoolMap $ playFieldState gs), block)
          landingEvent <- arr landBlock -< (block { position = newPosition }, (toBoolMap $ playFieldState gs))
          newGameState <- arr (buildGameState gs) -< block { position = newPosition }
          moveEvent <- arr moveBlock -< (buttonPresses, block { position = newPosition }, (playFieldState gs))
          returnA -< (newGameState, (gameModeEvent `lMerge` landingEvent `lMerge` moveEvent) `attach` rg)
        cont (Quitting, rg) =
          setBlockPosition rg (gs { finished = True })
        cont (Deleting indexes, rg) =
          let
            len = length indexes
            calcScore indexes = case len of
              0 -> 0
              1 -> 1
              2 -> 3
              3 -> 5
              4 -> 8
            playField = playFieldState gs
            updatedPlayField = (replicate len blankRow) ++ (foldr removeRow playField indexes)
            updatedGameState = gs { playFieldState = updatedPlayField, score = (score gs) + calcScore indexes }
          in
          pause (foldr replaceWithBlankRow gs indexes) (Yampa.localTime >>^ (< 0.4)) (setBlockPosition rg updatedGameState)
        cont (Running, rg) =
          setBlockPosition rg gs
        cont (Landing positions, rg) =
          let
            block = currentBlock gs
            newShape = nextBlockShape gs
            (newNextBlock, rg') = randomBlock rg
            updatedGameState =
              gs { currentBlock = block { blockShape = newShape, position = NoPosition }, nextBlockShape = newNextBlock, speed = min 10.0 ((speed gs) + 0.1) }
            flashingGs =
              gs { currentBlock = block { flashing = True } }
          in
          pause flashingGs (Yampa.localTime >>^ (< 0.4)) (setBlockPosition rg' (foldr (placeSquare (colourOf block)) updatedGameState positions))
        cont (BlockMove placedBlock, rg) =
          setBlockPosition rg (gs { currentBlock = placedBlock })
        cont (GameOver, rg) =
          let
            (blockShape, rg') = randomBlock rg
            (nextBlockShape, newRg) = randomBlock rg'
            hiddenBlock = (currentBlock gs) { position = NoPosition }
          in
          pause (gs { currentBlock = hiddenBlock, playFieldState = gameOverFieldState }) (Yampa.localTime >>^ (< 3)) (setBlockPosition newRg (initialGameState blockShape nextBlockShape))

computeGameMode :: (ButtonPresses, PlayFieldBoolMap, PlacedBlock) -> Yampa.Event GameEvent
computeGameMode (bp, field, block@(PlacedBlock { blockShape = shape, position = position })) =
  let
    fullRows =
      filter (\(idx, _) -> idx > 1 && idx < 22) $ filter isFullRow (indexed field)
    isFullRow (_, list) = all id (slice 0 9 list)
  in
  if quitKey bp then
    Yampa.Event Quitting
  else if fullRows /= [] then
    Yampa.Event (Deleting (map fst fullRows))
  else if position == NoPosition then
    Yampa.Event (BlockMove (PlacedBlock shape Deg0 (Position 3 2) False))
  else if not (canMoveTo block field) then
    Yampa.Event GameOver
  else
    Yampa.NoEvent

placeSquare :: BlockColour -> BlockPosition -> GameState -> GameState
placeSquare _ NoPosition gs = gs
placeSquare colour (Position x y) gs =
  let
    intY = floor y
    playField = playFieldState gs
    -- TODO: introduce lens for this kind of stuff?
    row = fromMaybe [] $ Safe.head $ slice intY intY playField
    (beforeX, xAndAfter) = splitAt x row
    newRow = beforeX ++ [colour] ++ (fromMaybe [] $ Safe.tail xAndAfter)
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

landBlock :: (PlacedBlock, PlayFieldBoolMap) -> Yampa.Event GameEvent
landBlock (PlacedBlock { position = NoPosition }, playFieldBoolMap) = Yampa.NoEvent
landBlock (block@(PlacedBlock { position = Position x y }), playFieldBoolMap) =
  let
    blockStopped = not $ canMoveTo block playFieldBoolMap
    landingBlocks = blocksToKeep blockStopped block
  in
  if blockStopped then
    Yampa.Event (Landing landingBlocks)
  else
    Yampa.NoEvent

moveBlock :: (ButtonPresses, PlacedBlock, PlayFieldState) -> Yampa.Event GameEvent
moveBlock (_, PlacedBlock { position = NoPosition }, _) = Yampa.NoEvent
moveBlock (buttons, block@(PlacedBlock { position = (Position x y), blockShape = shape, orientation = orientation }), playFieldState) =
  let
    playFieldBoolMap = (toBoolMap playFieldState)
    canMoveDown = canMoveTo (block { position = (Position x (y + 1)) }) playFieldBoolMap
    hardDroppedPosition = calcDroppedPosition playFieldBoolMap (Position x y)
    calcDroppedPosition field (Position x' y') =
      let
        movedBlock = block { position = (Position x' (y' + 1)) }
      in
      if canMoveTo movedBlock field then
        calcDroppedPosition field (position movedBlock)
      else
        Position x' y'
  in
  if hardDrop buttons && canMoveDown then
    Yampa.Event $ BlockMove (block { position = hardDroppedPosition })
  else if rotateL buttons && canRotateL block playFieldBoolMap then
    Yampa.Event $ BlockMove (block { orientation = spinLeft orientation })
  else if rotateR buttons && canRotateR block playFieldBoolMap then
    Yampa.Event $ BlockMove (block { orientation = spinRight orientation })
  else if leftArrow buttons && canMoveLeft block playFieldBoolMap then
    Yampa.Event $ BlockMove (block { position = (Position (x - 1) y) })
  else if rightArrow buttons && canMoveRight block playFieldBoolMap then
    Yampa.Event $ BlockMove (block { position = (Position (x + 1) y) })
  else if downArrow buttons && canMoveDown then
    Yampa.Event $ BlockMove (block { position = (Position x (y + 1)) })
  else if not canMoveDown then
    Yampa.Event $ BlockMove block
  else
    Yampa.NoEvent

-- TODO: Can I stop hard coding range... maybe count sum type?
randomBlock :: RandomGen rg => rg -> (BlockShape, rg)
randomBlock rg =
  let
    (i, newRg) = randomR (0::Int, 6::Int) rg
  in
  case i of
    0 -> (O, newRg)
    1 -> (I, newRg)
    2 -> (J, newRg)
    3 -> (L, newRg)
    4 -> (S, newRg)
    5 -> (T, newRg)
    6 -> (Z, newRg)

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

canMoveTo :: PlacedBlock -> PlayFieldBoolMap -> Bool
canMoveTo (PlacedBlock { position = NoPosition }) playFieldBoolMap = False
canMoveTo (block@(PlacedBlock { position = Position x y, blockShape = shape, orientation = orientation })) playFieldBoolMap =
  let
    intY = floor y
    rows = slice intY (intY + 3) (playFieldBoolMap ++ [(replicate 10 True)]) -- adding the floor (10 Trues)
    -- Trues are walls, Falses needed to allow 'I' block to go to the edges
    augmentedRows = map (\row -> [False, True] ++ row ++ [True, False]) rows
    cells = concatMap (slice (x + 2) (x + 5)) augmentedRows
    blockCells = concat $ get4x4 shape orientation
  in
  not $ any (\(l, r) -> l && r) (zip cells blockCells)

canMoveLeft :: PlacedBlock -> PlayFieldBoolMap -> Bool
canMoveLeft (PlacedBlock { position = NoPosition }) playFieldBoolMap = False
canMoveLeft (block@(PlacedBlock { position = Position x y })) playFieldBoolMap =
  canMoveTo (block { position = Position (x - 1) y }) playFieldBoolMap

canMoveRight :: PlacedBlock -> PlayFieldBoolMap -> Bool
canMoveRight (PlacedBlock { position = NoPosition }) playFieldBoolMap = False
canMoveRight (block@(PlacedBlock { position = Position x y })) playFieldBoolMap =
  canMoveTo (block { position = Position (x + 1) y }) playFieldBoolMap

canRotateL :: PlacedBlock -> PlayFieldBoolMap -> Bool
canRotateL (block@(PlacedBlock { orientation = orientation })) playFieldBoolMap =
  canMoveTo (block { orientation = spinLeft orientation }) playFieldBoolMap

canRotateR :: PlacedBlock -> PlayFieldBoolMap -> Bool
canRotateR (block@(PlacedBlock { orientation = orientation })) playFieldBoolMap =
  canMoveTo (block { orientation = spinRight orientation }) playFieldBoolMap


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

initScreen :: IO Renderer
initScreen = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  return renderer


-- digits below
drawDigit :: Int -> Renderer -> (Int, Char) -> IO ()
drawDigit start renderer (idx, num) =
  drawNum num ((start + idx) * 20) 10 renderer

fI = fromIntegral
drawNum :: Char -> Int -> Int -> Renderer -> IO ()
drawNum '0' x y renderer =
  drawRect renderer (Just (Rectangle (P (V2 (fI x + 4) (fI y + 2))) (V2 11 15)))
drawNum '1' x y renderer =
  drawLines renderer (fromList [(P (V2 (fI x + 6) (fI y + 4))), (P (V2 (fI x + 11) (fI y + 2))), (P (V2 (fI x + 11) (fI y + 16)))])
drawNum '2' x y renderer =
  drawLines renderer (fromList [(P (V2 (fI x + 4) (fI y + 2))), (P (V2 (fI x + 16) (fI y + 2))), (P (V2 (fI x + 16) (fI y + 8))), (P (V2 (fI x + 4) (fI y + 8))), (P (V2 (fI x + 4) (fI y + 16))), (P (V2 (fI x + 16) (fI y + 16)))])
drawNum '3' x y renderer =
  drawLines renderer (fromList [(P (V2 (fI x + 4) (fI y + 2))), (P (V2 (fI x + 16) (fI y + 2))), (P (V2 (fI x + 16) (fI y + 8))), (P (V2 (fI x + 4) (fI y + 8))), (P (V2 (fI x + 16) (fI y + 8))), (P (V2 (fI x + 16) (fI y + 16))), (P (V2 (fI x + 4) (fI y + 16)))])
drawNum '4' x y renderer =
  drawLines renderer (fromList [(P (V2 (fI x + 15) (fI y + 10))), (P (V2 (fI x + 3) (fI y + 10))), (P (V2 (fI x + 11) (fI y + 2))), (P (V2 (fI x + 11) (fI y + 16)))])
drawNum '5' x y renderer =
  drawLines renderer (fromList [(P (V2 (fI x + 16) (fI y + 2))), (P (V2 (fI x + 4) (fI y + 2))), (P (V2 (fI x + 4) (fI y + 8))), (P (V2 (fI x + 16) (fI y + 8))), (P (V2 (fI x + 16) (fI y + 16))), (P (V2 (fI x + 4) (fI y + 16)))])
drawNum '6' x y renderer =
  drawLines renderer (fromList [(P (V2 (fI x + 16) (fI y + 2))), (P (V2 (fI x + 4) (fI y + 2))), (P (V2 (fI x + 4) (fI y + 8))), (P (V2 (fI x + 16) (fI y + 8))), (P (V2 (fI x + 16) (fI y + 16))), (P (V2 (fI x + 4) (fI y + 16))), (P (V2 (fI x + 4) (fI y + 8)))])
drawNum '7' x y renderer =
  drawLines renderer (fromList [(P (V2 (fI x + 4) (fI y + 2))), (P (V2 (fI x + 16) (fI y + 2))), (P (V2 (fI x + 6) (fI y + 16)))])
drawNum '8' x y renderer =
  drawLines renderer (fromList [(P (V2 (fI x + 16) (fI y + 8))), (P (V2 (fI x + 16) (fI y + 2))), (P (V2 (fI x + 4) (fI y + 2))), (P (V2 (fI x + 4) (fI y + 8))), (P (V2 (fI x + 16) (fI y + 8))), (P (V2 (fI x + 16) (fI y + 16))), (P (V2 (fI x + 4) (fI y + 16))), (P (V2 (fI x + 4) (fI y + 8)))])
drawNum '9' x y renderer =
  drawLines renderer (fromList [(P (V2 (fI x + 16) (fI y + 8))), (P (V2 (fI x + 16) (fI y + 2))), (P (V2 (fI x + 4) (fI y + 2))), (P (V2 (fI x + 4) (fI y + 8))), (P (V2 (fI x + 16) (fI y + 8))), (P (V2 (fI x + 16) (fI y + 16))), (P (V2 (fI x + 4) (fI y + 16)))])
drawNum _ x y renderer =
  fillRect renderer (Just (Rectangle (P (V2 (fI x + 4) (fI y + 2))) (V2 12 16)))

-- block shapes below
get4x4 :: BlockShape -> BlockOrientation -> [[Bool]]
get4x4 O _ =
  [ [False, True, True, False]
  , [False, True, True, False]
  , [False, False, False, False]
  , [False, False, False, False]
  ]

get4x4 I Deg0 =
  [ [False, False, False, False]
  , [True, True, True, True]
  , [False, False, False, False]
  , [False, False, False, False]
  ]
get4x4 I Deg90 =
  [ [False, True, False, False]
  , [False, True, False, False]
  , [False, True, False, False]
  , [False, True, False, False]
  ]
get4x4 I Deg180 =
  [ [False, False, False, False]
  , [False, False, False, False]
  , [True, True, True, True]
  , [False, False, False, False]
  ]
get4x4 I Deg270 =
  [ [False, False, True, False]
  , [False, False, True, False]
  , [False, False, True, False]
  , [False, False, True, False]
  ]

get4x4 J Deg0 =
  [ [True, False, False, False]
  , [True, True, True, False]
  , [False, False, False, False]
  , [False, False, False, False]
  ]
get4x4 J Deg90 =
  [ [False, True, True, False]
  , [False, True, False, False]
  , [False, True, False, False]
  , [False, False, False, False]
  ]
get4x4 J Deg180 =
  [ [False, False, False, False]
  , [True, True, True, False]
  , [False, False, True, False]
  , [False, False, False, False]
  ]
get4x4 J Deg270 =
  [ [False, True, False, False]
  , [False, True, False, False]
  , [True, True, False, False]
  , [False, False, False, False]
  ]

get4x4 L Deg0 =
  [ [False, False, True, False]
  , [True, True, True, False]
  , [False, False, False, False]
  , [False, False, False, False]
  ]
get4x4 L Deg90 =
  [ [False, True, False, False]
  , [False, True, False, False]
  , [False, True, True, False]
  , [False, False, False, False]
  ]
get4x4 L Deg180 =
  [ [False, False, False, False]
  , [True, True, True, False]
  , [True, False, False, False]
  , [False, False, False, False]
  ]
get4x4 L Deg270 =
  [ [True, True, False, False]
  , [False, True, False, False]
  , [False, True, False, False]
  , [False, False, False, False]
  ]

get4x4 S Deg0 =
  [ [False, True, True, False]
  , [True, True, False, False]
  , [False, False, False, False]
  , [False, False, False, False]
  ]
get4x4 S Deg90 =
  [ [False, True, False, False]
  , [False, True, True, False]
  , [False, False, True, False]
  , [False, False, False, False]
  ]
get4x4 S Deg180 =
  [ [False, False, False, False]
  , [False, True, True, False]
  , [True, True, False, False]
  , [False, False, False, False]
  ]
get4x4 S Deg270 =
  [ [True, False, False, False]
  , [True, True, False, False]
  , [False, True, False, False]
  , [False, False, False, False]
  ]

get4x4 T Deg0 =
  [ [False, True, False, False]
  , [True, True, True, False]
  , [False, False, False, False]
  , [False, False, False, False]
  ]
get4x4 T Deg90 =
  [ [False, True, False, False]
  , [False, True, True, False]
  , [False, True, False, False]
  , [False, False, False, False]
  ]
get4x4 T Deg180 =
  [ [False, False, False, False]
  , [True, True, True, False]
  , [False, True, False, False]
  , [False, False, False, False]
  ]
get4x4 T Deg270 =
  [ [False, True, False, False]
  , [True, True, False, False]
  , [False, True, False, False]
  , [False, False, False, False]
  ]

get4x4 Z Deg0 =
  [ [True, True, False, False]
  , [False, True, True, False]
  , [False, False, False, False]
  , [False, False, False, False]
  ]
get4x4 Z Deg90 =
  [ [False, False, True, False]
  , [False, True, True, False]
  , [False, True, False, False]
  , [False, False, False, False]
  ]
get4x4 Z Deg180 =
  [ [False, False, False, False]
  , [True, True, False, False]
  , [False, True, True, False]
  , [False, False, False, False]
  ]
get4x4 Z Deg270 =
  [ [False, True, False, False]
  , [True, True, False, False]
  , [True, False, False, False]
  , [False, False, False, False]
  ]
