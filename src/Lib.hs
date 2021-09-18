{-# LANGUAGE OverloadedStrings #-}

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
  , startTime :: UTCTime
  , timePassed :: DTime
  , frameNum :: Int
  , buttonPresses :: ButtonPresses
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

runGame :: IO ()
runGame = do
  renderer <- initScreen
  t <- getCurrentTime
  timeRef <- newIORef (t, t)
  reactimate initialise (input timeRef) (output renderer) process

initialise :: IO GameState
initialise = do
  putStrLn "Starting..."
  currTime <- getCurrentTime
  -- let timePassed = diffUTCTime currTime (startTime gs)
  -- putStrLn ("frameChanged: " ++ (show (frameChanged gs)) ++ ", " ++ (show timePassed))
  return GameState { finished = False, startTime = currTime, timePassed = 0, frameNum = 0, buttonPresses = noButtonPressed }

input :: IORef (UTCTime, UTCTime) -> Bool -> IO (DTime, Maybe GameState)
input ref _ = do
  currTime <- getCurrentTime
  (start, lastTime) <- readIORef ref
  writeIORef ref (start, currTime)
  events <- pollEvents
  let dt = diffUTCTime currTime lastTime
      timePassed = realToFrac (diffUTCTime currTime start)
      frameNum = (round (timePassed * 60)) `mod` 60
      buttonPresses = buttonPressesFrom events
      gameState = GameState {finished = False, startTime = start, timePassed = timePassed, frameNum = frameNum, buttonPresses = buttonPresses}
  return (realToFrac dt, Just gameState)

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
  fillRect renderer (Just (Rectangle (P (V2 100 100)) (V2 20 20)))
  present renderer
  currTime <- getCurrentTime
  let tp = timePassed gs
      bps = buttonPresses gs
  putStrLn ("timePassed: " ++ (show tp) ++ ", frameNum: " ++ (show $ frameNum gs))
  when (bps /= noButtonPressed) (putStrLn ("Buttons: " ++ (show $ bps)))
  when (finished gs) (putStrLn "Done")
  return (finished gs)

process :: SF GameState GameState
-- time :: SF a Time, arr 'lifts' a regular func to SF, >>> is SF bind
process =
    (arr dup) >>> (Yampa.identity *** Yampa.time) >>> arr (\(gs, t) ->
                       let gameOver = t > 4
                       in gs{ finished = gameOver })

initScreen :: IO Renderer
initScreen = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  return renderer


