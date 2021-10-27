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
  deriving (Show)

data BlockShape
  = O
  | I
  deriving (Show)

data BlockOrientation
  = LeftTilt
  | UpsideDown
  | RightTilt
  | Upright
  deriving (Show)

type BlockPosition = (Int, Float)

type PlayFieldState = [[Bool]]

-- play field is 20 x 10. There are extra rows at the top (think row -1 and -2)
initialPlayFieldState :: PlayFieldState
initialPlayFieldState =
  -- extra False added at the right end so 'I' block can go to the right most column
  ((replicate 22 $ blankRow) ++ [(replicate 12 True)])

blankRow :: [Bool]
blankRow = ([True] ++ (replicate 10 False) ++ [True, False])

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

get4x4 :: BlockShape -> BlockOrientation -> [[Bool]]
get4x4 O _ = [ [False, False, False, False], [False, True, True, False], [False, True, True, False], [False, False, False, False]]
get4x4 I _ = [ [False, True, False, False], [False, True, False, False], [False, True, False, False], [False, True, False, False]]

drawBlock :: PlacedBlock -> Renderer -> IO ()
drawBlock PlacedBlock { blockShape = shape, orientation = orientation, position = (x, y)} renderer =
  let
    blockMap = get4x4 shape orientation
    renderRow position (idy, row) = sequence $ map (renderCell (fromIntegral idy)) (indexed row)
    renderCell idy (idx, blockExists) = when blockExists (drawSquare (x + idx, y + idy) renderer)
  in
  sequence_ $ map (renderRow position) (indexed  blockMap)

drawSquare :: BlockPosition -> Renderer -> IO ()
drawSquare (x, y) renderer = do
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
              when cell (drawSquare (idx - 1, (fromIntegral idy)) renderer)
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

buildGameState :: GameState -> (PlacedBlock, GameMode) -> GameState
buildGameState initialState (block, mode) =
  initialState { finished = (mode == Quitting)
     , currentBlock = block
  }

data GameMode
  = Running
  | Quitting
  | Deleting [Int]
  -- | Deleted [Int]
  deriving (Show, Eq)

setBlockPosition :: RandomGen rg => rg -> GameState -> SF ([SDL.Event], GameMode) GameState
setBlockPosition rg gs = switch (sf >>> second notYet) cont
  where sf = proc (events, t) -> do
          let buttonPresses = buttonPressesFrom events
              block = currentBlock gs
              (x, y) = position $ block
          dy <- integral -< (1.0 :: Float)
          newY <- arr (\(a, b) -> (a + b)) -< (y, dy)
          -- if there's a full row, mode turns into 'deleting'
          gameMode <- arr computeGameMode -< (buttonPresses, (playFieldState gs))
          newGameState <- arr (buildGameState gs) -< (block { position = (x, newY) }, gameMode)
          now <- localTime -< ()
          moveEvent <- arr moveBlock -< (rg, buttonPresses, block { position = (x, newY) }, (playFieldState gs))
          returnA -< (newGameState, moveEvent `attach` gameMode)
        cont ((block@(PlacedBlock { position = (x, y)}), keepBlocksAt, newRg), mode) =
          let
            newGameState = buildGameState gs (block, mode)
          in
          -- if mode is 'deleting' blacken rows and pause for 1 second, then remove rows
          case Debug.Trace.trace ("mode is: " ++ (show mode)) mode of
            Deleting indexes ->
              let
                playField = playFieldState newGameState
                updatedPlayField = (replicate (length indexes) blankRow ) ++ (foldr removeRow playField indexes)
                updatedGameState = newGameState { playFieldState = updatedPlayField }
              in
              pause (foldr replaceWithBlankRow newGameState indexes) (Yampa.localTime >>^ (< 1.0)) (setBlockPosition newRg (Debug.Trace.trace ("playField: " ++ (show (playFieldState updatedGameState))) updatedGameState))
            Quitting ->
              setBlockPosition newRg newGameState
            Running ->
              case keepBlocksAt of
                [] ->
                  setBlockPosition newRg newGameState
                positions ->
                  pause gs (Yampa.localTime >>^ (< 1.0)) (setBlockPosition newRg (foldr placeSquare newGameState positions))

computeGameMode :: (ButtonPresses, PlayFieldState) -> GameMode
computeGameMode (bp, field) =
  let
    fullRows =
      filter (\(idx, _) -> idx > 1 && idx < 22) $ filter isFullRow (indexed field)
    isFullRow (_, list) = all id (slice 1 10 list)
  in
  if quitKey bp then
    Quitting
  else if fullRows /= [] then
    Deleting (map fst fullRows)
  else
    Running

placeSquare :: BlockPosition -> GameState -> GameState
placeSquare (x, y) gs =
  let
    intY = floor y
    playField = playFieldState gs
    -- TODO: introduce lens for this kind of stuff?
    row = fromMaybe [] $ Safe.head $ slice intY intY playField
    (beforeX, xAndAfter) = splitAt (x + 1) row
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

moveBlock :: RandomGen rg => (rg, ButtonPresses, PlacedBlock, PlayFieldState) -> Yampa.Event (PlacedBlock, [BlockPosition], rg)
moveBlock (rg, buttons, block@(PlacedBlock { position = (x, y) }), playFieldState) =
  let
    blockStopped = not $ canMoveTo block playFieldState
    cannotMoveDown = not $ canMoveTo (block { position = (x, y + 1) }) playFieldState
    (newX, newY, (newShape, newOrientation, newRg)) =
      if blockStopped then
        (3, 2, randomBlock rg) -- Note the 'top' is 2, not 0
      else
        (x, y, (blockShape block, orientation block, rg))
    keepBlocksAt = blocksToKeep blockStopped block
  in
  if blockStopped then
    Yampa.Event (block { position = (newX, newY), blockShape = newShape, orientation = newOrientation }, keepBlocksAt, newRg)
  else if (leftArrow buttons) && (canMoveLeft block playFieldState) then
    Yampa.Event (block { position = (newX - 1, newY), blockShape = newShape, orientation = newOrientation }, keepBlocksAt, newRg)
  else if (rightArrow buttons) && (canMoveRight block playFieldState) then
    Yampa.Event (block { position = (newX + 1, newY), blockShape = newShape, orientation = newOrientation }, keepBlocksAt, newRg)
  else if cannotMoveDown then -- if already bottom, don't check downArrow below
    Yampa.Event (block { position = (newX, newY), blockShape = newShape, orientation = newOrientation }, keepBlocksAt, newRg)
  else if downArrow buttons then
    Yampa.Event (block { position = (newX, newY + 1), blockShape = newShape, orientation = newOrientation }, keepBlocksAt, newRg)
  else
    Yampa.NoEvent

-- TODO: Stop hard coding range... count enum?
randomBlock :: RandomGen rg => rg -> (BlockShape, BlockOrientation, rg)
randomBlock rg =
  let
    (i, newRg) = randomR (0::Int, 1::Int) rg
  in
  if i == 0 then
    (I, Upright, newRg)
  else
    (O, Upright, newRg)



blocksToKeep :: Bool -> PlacedBlock -> [BlockPosition]
blocksToKeep blockStopped block@(PlacedBlock { position = (x, overlappingY), blockShape = shape, orientation = orientation }) =
  let
    y = overlappingY - 1
    blockMap = get4x4 shape orientation
  in
  if blockStopped then
    concatMap (\(idy, row) ->
      map (\(idx, _) ->
        (x + idx, y + (fromIntegral idy))
      ) (filter (\(_, v) -> v)
        (indexed row)
      )
    ) (indexed blockMap)
  else
    []

canMoveTo :: PlacedBlock -> PlayFieldState -> Bool
canMoveTo (block@(PlacedBlock { position = (x, y), blockShape = shape, orientation = orientation })) playFieldState =
  let
    intY = floor y
    rows = slice intY (intY + 3) playFieldState
    cells = concatMap (slice (x + 1) (x + 4)) rows
    blockCells = concat $ get4x4 shape orientation
  in
  not $ any (\(l, r) -> l && r) (zip cells blockCells)

canMoveLeft :: PlacedBlock -> PlayFieldState -> Bool
canMoveLeft (block@(PlacedBlock { position = (x, y) })) playFieldState =
  canMoveTo (block { position = (x - 1, y) }) playFieldState

canMoveRight :: PlacedBlock -> PlayFieldState -> Bool
canMoveRight (block@(PlacedBlock { position = (x, y) })) playFieldState =
  canMoveTo (block { position = (x + 1, y) }) playFieldState

defaultBlock :: PlacedBlock
defaultBlock =
  PlacedBlock
    { blockShape = O
    , orientation = Upright
    , position = (2, 2)
    }

initScreen :: IO Renderer
initScreen = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  return renderer


