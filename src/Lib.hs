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
  , timePassed :: DTime
  , currentBlock :: PlacedBlock
  , playFieldState :: PlayFieldState
  }

initialGameState :: GameState
initialGameState =
  GameState False 0 defaultBlock initialPlayFieldState

data ButtonPresses = ButtonPresses
  { upArrow :: Bool
  , downArrow :: Bool
  , leftArrow :: Bool
  , rightArrow :: Bool
  , rotateL :: Bool
  , rotateR :: Bool
  , hardDrop :: Bool
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
  ((replicate 22 $ ([True] ++ (replicate 10 False) ++ [True]))) ++ [(replicate 12 True)] ++ [(replicate 12 True)]

runGame :: IO ()
runGame = do
  renderer <- initScreen
  t <- getCurrentTime
  timeRef <- newIORef (t, t)
  reactimate initialise (input timeRef) (output renderer) (process initialGameState)

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
  in
  ButtonPresses u d l r shift ctrl sp

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
  drawBorders renderer
  present renderer
  currTime <- getCurrentTime
  -- putStrLn ("currentBlock: " ++ (show (currentBlock gs)))
  when (finished gs) (putStrLn "Done")
  return (finished gs)

get4x4 :: BlockShape -> BlockOrientation -> [[Bool]]
get4x4 O _ = [ [False, False, False, False], [False, True, True, False], [False, True, True, False], [False, False, False, False]]

drawBlock :: PlacedBlock -> Renderer -> IO ()
drawBlock PlacedBlock { blockShape = shape, orientation = orientation, position = (x, y)} renderer =
  let
    blockMap = get4x4 shape orientation
    renderRow position (idy, row) = sequence $ map (renderCell (fromIntegral idy)) (indexed row)
    renderCell idy (idx, blockExists) = when blockExists (drawSquare (x + idx, y + idy) renderer)
  in
  sequence_ $ map (renderRow position) (indexed blockMap)

drawSquare :: BlockPosition -> Renderer -> IO ()
drawSquare (x, y) renderer = do
  rendererDrawColor renderer $= V4 192 32 32 255
  fillRect renderer (Just (Rectangle (P (V2 (fromIntegral (x*20+40)) (fromIntegral ((floor y)*20+40)))) (V2 20 20)))

drawPlayField :: PlayFieldState -> Renderer -> IO ()
drawPlayField playField renderer = do
  sequence_ $
    map
      (\(idy, row) ->
        sequence $
          map
            (\(idx, cell) ->
              when cell (drawSquare (idx - 1, (fromIntegral idy) - 2) renderer)
            )
            (indexed row)
      )
      (indexed playField)

drawBorders :: Renderer -> IO ()
drawBorders renderer =
  let playFieldRect = Rectangle (P (V2 40 40)) (V2 200 400)
  in
  drawRect renderer (Just playFieldRect)

process :: GameState -> SF [SDL.Event] GameState
process initialState =
    (Yampa.identity &&& Yampa.time) >>> (setBlockPosition initialState)

buildGameState :: GameState -> (BlockPosition, DTime) -> GameState
buildGameState initialState (position, t) =
  let oldBlock = currentBlock initialState
      updatedBlock = oldBlock { position = position }
  in
  initialState { finished = t > 30
     , timePassed = t
     , currentBlock = updatedBlock
  }

placeBlock :: BlockPosition -> GameState -> GameState
placeBlock (x, y) gs =
  let
    intY = floor y
    playField = playFieldState gs
    -- TODO: introduce lens for this kind of stuff?
    row = fromMaybe [] $ Safe.head $ slice (intY + 2) (intY + 2) playField
    (beforeX, xAndAfter) = splitAt (x + 1) row
    newRow = beforeX ++ [True] ++ (fromMaybe [] $ Safe.tail xAndAfter)
    (beforeY, yAndAfter) = splitAt (intY + 2) playField
    newPlayField = beforeY ++ [newRow] ++ (fromMaybe [] $ Safe.tail yAndAfter)
  in
    gs { playFieldState = newPlayField }



setBlockPosition :: GameState -> SF ([SDL.Event], Time) GameState
setBlockPosition gs = switch (sf >>> second notYet) cont
  where sf = proc (events, t) -> do
          let buttonPresses = buttonPressesFrom events
              (x, y) = position $ currentBlock gs
          dy <- integral -< (1.0 :: Float)
          newY <- arr (\(a, b) -> (a + b)) -< (y, dy)
          newGameState <- arr (buildGameState gs) -< ((x, newY), t)
          now <- localTime -< ()
          moveEvent <- arr moveBlock -< (buttonPresses, (x, newY), (playFieldState gs))
          returnA -< (newGameState, moveEvent `attach` now)
        cont (((x, y), keepBlockAt), t) =
          let
            newGameState = buildGameState gs ((x, y), t)
          in
          case keepBlockAt of
            Just pos ->
              pause gs (Yampa.localTime >>^ (< 1.0)) (setBlockPosition (placeBlock pos newGameState))
            Nothing ->
              setBlockPosition newGameState


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

moveBlock :: (ButtonPresses, BlockPosition, PlayFieldState) -> Yampa.Event (BlockPosition, Maybe BlockPosition)
moveBlock (buttons, (x, y), playFieldState) =
  let blockStopped = not $ canMoveTo (x, y) playFieldState
      cannotMoveDown = not $ canMoveTo (x, y+1) playFieldState
      newY = if (Debug.Trace.trace ("blockStopped: " ++ (show blockStopped)) blockStopped) then 0 else y
      keepBlockAt = if blockStopped then Just (x, y - 1) else Nothing
  in
  if (leftArrow buttons) && (canMoveLeft (x, y) playFieldState) then
    Yampa.Event ((x - 1, newY), keepBlockAt)
  else if (rightArrow buttons) && (canMoveRight (x, y) playFieldState) then
    Yampa.Event ((x + 1, newY), keepBlockAt)
  else if cannotMoveDown then -- if already bottom, don't check downArrow below
    Yampa.Event ((x, newY), keepBlockAt)
  else if downArrow buttons then
    Yampa.Event ((x, newY + 1), keepBlockAt)
  else
    Yampa.NoEvent

data MoveDirection
  = Left
  | Right

canMoveTo :: BlockPosition -> PlayFieldState -> Bool
canMoveTo (x, y) playFieldState =
  let
    intY = floor y
    row = fromMaybe [] $ Safe.head $ slice (intY + 2) (intY + 2) playFieldState
    cell = fromMaybe False $ Safe.head $ slice (x + 1) (x + 1) row
  in
  not cell

canMoveLeft :: BlockPosition -> PlayFieldState -> Bool
canMoveLeft (x, y) playFieldState =
  canMoveTo (x - 1, y) playFieldState

canMoveRight :: BlockPosition -> PlayFieldState -> Bool
canMoveRight (x, y) playFieldState =
  canMoveTo (x + 1, y) playFieldState

defaultBlock :: PlacedBlock
defaultBlock =
  PlacedBlock
    { blockShape = O
    , orientation = Upright
    , position = (2, 0)
    }

initScreen :: IO Renderer
initScreen = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  return renderer


