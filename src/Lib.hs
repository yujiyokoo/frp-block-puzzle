{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Lib
    ( runGame
    ) where

import Control.Monad
import Data.IORef
import Data.Time.Clock
import FRP.Yampa as Yampa
import SDL as SDL
import SDL.Vect

data GameState = GameState
  { finished :: Bool
  , timePassed :: DTime
  , currentBlock :: PlacedBlock
  }

initialGameState :: GameState
initialGameState =
  GameState False 0 defaultBlock

data ButtonPresses = ButtonPresses
  { upArrow :: Bool
  , downArrow :: Bool
  , leftArrow :: Bool
  , rightArrow :: Bool
  , rotateL :: Bool
  , rotateR :: Bool
  , drop :: Bool
  }
  deriving (Show, Eq)

noButtonPressed :: ButtonPresses
noButtonPressed = ButtonPresses False False False False False False False

data GameScreen = GameScreen
  { playFieldLeft :: Int
  , playFieldTop :: Int
  }

defaultGameScreen :: GameScreen
defaultGameScreen = GameScreen
  { playFieldLeft = 40
  , playFieldTop = 40
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

type BlockPosition = (Int, Int)

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
  -- draw a red square
  rendererDrawColor renderer $= V4 192 32 32 255
  drawBlock (position (currentBlock gs)) renderer
  drawBorders renderer
  present renderer
  currTime <- getCurrentTime
  let tp = timePassed gs
  putStrLn ("currentBlock: " ++ (show (currentBlock gs)))
  when (finished gs) (putStrLn "Done")
  return (finished gs)

drawBlock :: BlockPosition -> Renderer -> IO ()
drawBlock (x, y) renderer =
  fillRect renderer (Just (Rectangle (P (V2 (fromIntegral (x*20+100)) (fromIntegral (y*20+100)))) (V2 20 20)))

drawBorders :: Renderer -> IO ()
drawBorders renderer =
  let playFieldRect = Rectangle (P (V2 (fromIntegral $ playFieldLeft defaultGameScreen) (fromIntegral $ playFieldTop defaultGameScreen))) (V2 200 400)
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
  initialState { finished = t > 4
     , timePassed = t
     , currentBlock = updatedBlock
  }

setBlockPosition :: GameState -> SF ([SDL.Event], Time) GameState
setBlockPosition gs = switch (sf  >>> second notYet) cont
  where sf = proc (events, t) -> do
          let buttonPresses = buttonPressesFrom events
              (x, y) = position $ currentBlock gs
          dy <- round ^<< integral -< (1.0 :: Float)
          newY <- arr (\(a, b) -> a + b) -< (y, dy)
          newGameState <- arr (buildGameState gs) -< ((x, newY), t)
          inputEvent <- arr (\(bps, y) -> if leftArrow bps then Yampa.Event y else Yampa.NoEvent) -< (buttonPresses, newY)
          returnA -< (newGameState, inputEvent)
        cont y =
          let
            (oldX, _) = position $ currentBlock gs
            newGameState = buildGameState gs ((oldX - 1, y), 0)
          in
          setBlockPosition newGameState


defaultBlock :: PlacedBlock
defaultBlock =
  PlacedBlock
    { blockShape = O
    , orientation = Upright
    , position = (0, 0)
    }

initScreen :: IO Renderer
initScreen = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  return renderer


