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
  }

initialGameState :: BlockShape -> BlockShape -> GameState
initialGameState shape nextShape =
  let
    initialBlock = PlacedBlock shape Deg0 (Position 3 2)
  in
  GameState False initialBlock nextShape initialPlayFieldState 0

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

type PlayFieldState = [[Bool]]

-- play field is 20 x 10. There are extra rows at the top (think row -1 and -2)
initialPlayFieldState :: PlayFieldState
initialPlayFieldState = replicate 22 blankRow

gameOverFieldState :: PlayFieldState
gameOverFieldState = (replicate 2 $ replicate 10 False) ++ (replicate 20 $ replicate 10 True)

blankRow :: [Bool]
blankRow = replicate 10 False

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
  rendererDrawColor renderer $= V4 32 32 32 255
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
    rendererDrawColor renderer $= V4 192 32 32 255
    sequence_ $
      map (drawDigit start renderer) (indexed digits)

renderNextBlock :: BlockShape -> Renderer -> IO ()
renderNextBlock shape renderer =
  drawBlock (PlacedBlock { blockShape = shape, orientation = Deg0, position = Position 14 4 }) renderer

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
  let
    leftX = x * 20 + 40
    topY = (floor y) * 20
  rendererDrawColor renderer $= V4 192 32 32 255
  fillRect renderer (Just (Rectangle (P (V2 (fI leftX) (fI topY))) (V2 20 20)))
  rendererDrawColor renderer $= V4 224 64 64 255
  drawLine renderer (P (V2 (fI leftX) (fI topY))) (P (V2 (fI (leftX + 19)) (fI topY)))
  drawLine renderer (P (V2 (fI leftX) (fI topY))) (P (V2 (fI leftX) (fI (topY + 19))))
  rendererDrawColor renderer $= V4 128 32 32 255
  drawLine renderer (P (V2 (fI (leftX + 1)) (fI (topY + 19)))) (P (V2 (fI (leftX + 19)) (fI (topY + 19))))
  drawLine renderer (P (V2 (fI (leftX + 19)) (fI (topY + 1)))) (P (V2 (fI (leftX + 19)) (fI (topY + 19))))
  rendererDrawColor renderer $= V4 224 96 96 255
  drawPoint renderer (P (V2 (fI leftX) (fI topY)))


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
drawBorders renderer = do
  let playFieldRect = Rectangle (P (V2 39 39)) (V2 201 401)
  rendererDrawColor renderer $= V4 192 32 32 255
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
          dy <- integral -< (1.0 :: Float)
          newPosition <- arr setY -< (pos, dy)
          gameModeEvent <- arr computeGameMode -< (buttonPresses, (playFieldState gs), block)
          landingEvent <- arr landBlock -< (block { position = newPosition }, (playFieldState gs))
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
            updatedGameState = gs { playFieldState = updatedPlayField, score = (score gs) + calcScore indexes}
          in
          pause (foldr replaceWithBlankRow gs indexes) (Yampa.localTime >>^ (< 0.4)) (setBlockPosition rg updatedGameState)
        cont (Running, rg) =
          setBlockPosition rg gs
        cont (Landing positions, rg) =
          let
            block = currentBlock gs
            newShape = nextBlockShape gs
            (newNextBlock, rg') = randomBlock rg
            gsWithHiddenBlock =
              gs { currentBlock = block { blockShape = newShape, position = NoPosition }, nextBlockShape = newNextBlock }
          in
          pause gs (Yampa.localTime >>^ (< 0.4)) (setBlockPosition rg' (foldr placeSquare gsWithHiddenBlock positions))
        cont (BlockMove placedBlock, rg) =
          setBlockPosition rg (gs { currentBlock = placedBlock })
        cont (GameOver, rg) =
          let
            (blockShape, rg') = randomBlock rg
            (nextBlockShape, newRg) = randomBlock rg'
          in
          pause (gs { playFieldState = gameOverFieldState }) (Yampa.localTime >>^ (< 3)) (setBlockPosition newRg (initialGameState blockShape nextBlockShape))

computeGameMode :: (ButtonPresses, PlayFieldState, PlacedBlock) -> Yampa.Event GameEvent
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
    Yampa.Event (BlockMove (PlacedBlock shape Deg0 (Position 3 2)))
  else if not (canMoveTo block field) then
    Yampa.Event GameOver
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

moveBlock :: (ButtonPresses, PlacedBlock, PlayFieldState) -> Yampa.Event GameEvent
moveBlock (_, PlacedBlock { position = NoPosition }, _) = Yampa.NoEvent
moveBlock (buttons, block@(PlacedBlock { position = (Position x y), blockShape = shape, orientation = orientation }), playFieldState) =
  let
    canMoveDown = canMoveTo (block { position = (Position x (y + 1)) }) playFieldState
    hardDroppedPosition = calcDroppedPosition playFieldState (Position x y)
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
  else if rotateL buttons && canRotateL block playFieldState then
    Yampa.Event $ BlockMove (block { orientation = spinLeft orientation })
  else if rotateR buttons && canRotateR block playFieldState then
    Yampa.Event $ BlockMove (block { orientation = spinRight orientation })
  else if leftArrow buttons && canMoveLeft block playFieldState then
    Yampa.Event $ BlockMove (block { position = (Position (x - 1) y) })
  else if rightArrow buttons && canMoveRight block playFieldState then
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
  not $ any (\(l, r) -> l && r) (zip cells blockCells)

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
