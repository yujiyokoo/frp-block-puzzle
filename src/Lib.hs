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
  , frameNum :: Int
  , buttonPresses :: ButtonPresses
  , currentBlock :: PlacedBlock
  }

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
  reactimate initialise (input timeRef) (output renderer) process

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
      bps = buttonPresses gs
  putStrLn ("timePassed: " ++ (show tp) ++ ", frameNum: " ++ (show $ frameNum gs))
  putStrLn ("currentBlock: " ++ (show (currentBlock gs)))
  when (bps /= noButtonPressed) (putStrLn ("Buttons: " ++ (show $ bps)))
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

process :: SF [SDL.Event] GameState
process =
    (Yampa.identity &&& Yampa.time) >>> arr updateGameState >>> overrideBlockPosition

overrideBlockPosition :: SF GameState GameState
overrideBlockPosition =
  proc gameState -> do
    -- let (x, y) = position (currentBlock gameState)
    newY <- round ^<< integral -< (1.0 :: Float)
    newBlock <- arr (\y -> defaultBlock { position = (0, y) }) -< newY
    returnA -< (gameState { currentBlock = newBlock } )

defaultBlock :: PlacedBlock
defaultBlock =
  PlacedBlock
    { blockShape = O
    , orientation = Upright
    , position = (0, 0)
    }

updateGameState :: ([SDL.Event], Time) -> GameState
updateGameState (events, t) =
  let
    buttonPresses = buttonPressesFrom events
    frameNum = (round (t * 60)) `mod` 60
  in
    GameState
      { finished = t > 4
      , timePassed = t
      , frameNum = frameNum
      , buttonPresses = buttonPresses
      , currentBlock = defaultBlock
      }

initScreen :: IO Renderer
initScreen = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  return renderer


