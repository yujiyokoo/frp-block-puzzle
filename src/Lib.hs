{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Lib
    ( runGame
    ) where

import Control.Monad
import Data.IORef
import Data.Time.Clock
import Data.List.Safe as Safe
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
  ((replicate 22 $ ([True] ++ (replicate 10 False) ++ [True]))) ++ [(replicate 12 True)]

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
  drawBlock (position (currentBlock gs)) renderer
  drawBorders renderer
  present renderer
  currTime <- getCurrentTime
  -- putStrLn ("currentBlock: " ++ (show (currentBlock gs)))
  when (finished gs) (putStrLn "Done")
  return (finished gs)

drawBlock :: BlockPosition -> Renderer -> IO ()
drawBlock (x, y) renderer = do
  rendererDrawColor renderer $= V4 192 32 32 255
  fillRect renderer (Just (Rectangle (P (V2 (fromIntegral (x*20+40)) (fromIntegral ((floor y)*20+40)))) (V2 20 20)))

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
        cont (((x, y), shouldPause), t) =
          let
            newGameState = buildGameState gs ((x, y), t)
          in
          if shouldPause then
            pause gs (Yampa.localTime >>^ (< 1.0)) (setBlockPosition newGameState)
          else
            setBlockPosition newGameState


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

moveBlock :: (ButtonPresses, BlockPosition, PlayFieldState) -> Yampa.Event (BlockPosition, Bool)
moveBlock (buttons, (x, y), playFieldState) =
  let bottomReached = (floor y) > 19 -- is y 20 or over?
      newY = if bottomReached then 0 else y
  in
  if (leftArrow buttons) && (canMoveLeft (x, y) playFieldState) then
    Yampa.Event ((x - 1, newY), bottomReached)
  else if (rightArrow buttons) && (canMoveRight (x, y) playFieldState) then
    Yampa.Event ((x + 1, newY), bottomReached)
  else if y > 19 then -- if already bottom, don't check downArrow below
    Yampa.Event ((x, newY), bottomReached)
  else if downArrow buttons then
    Yampa.Event ((x, newY + 1), bottomReached)
  else
    Yampa.NoEvent

canMoveLeft :: BlockPosition -> PlayFieldState -> Bool
canMoveLeft (x, y) playFieldState =
  let
    intY = floor y
    row = fromMaybe [] $ Safe.head $ slice (intY + 2) (intY + 2) playFieldState
    cell = fromMaybe False $ Safe.head $ slice x x row
  in
  Debug.Trace.trace ("row: " ++ (show row) ++ ", x: " ++ (show x)) (not cell)

canMoveRight :: BlockPosition -> PlayFieldState -> Bool
canMoveRight (x, y) playFieldState =
  let
    intY = floor y
    row = fromMaybe [] $ Safe.head $ slice (intY + 2) (intY + 2) playFieldState
    cell = fromMaybe False $ Safe.head $ slice (x + 2) (x + 2) row
  in
  Debug.Trace.trace ("row: " ++ (show row) ++ ", x: " ++ (show x)) (not cell)

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


