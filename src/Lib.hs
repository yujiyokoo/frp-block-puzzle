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
  }

runGame :: IO ()
runGame = do
  renderer <- initScreen
  t <- getCurrentTime
  timeRef <- newIORef t
  reactimate initialise (input timeRef) (output renderer) isTimeUp

initialise :: IO GameState
initialise = do
  putStrLn "Starting..."
  return GameState { finished = False }

input :: IORef UTCTime -> Bool -> IO (DTime, Maybe GameState)
input ref _ = do
  currTime <- getCurrentTime
  lastTime <- readIORef ref
  writeIORef ref currTime
  let dt = diffUTCTime currTime lastTime
  return (realToFrac dt, Nothing)

output :: Renderer -> Bool -> GameState -> IO Bool
output renderer _ gs = do
  events <- pollEvents
  rendererDrawColor renderer $= V4 32 32 32 255
  clear renderer
  -- draw a red square
  rendererDrawColor renderer $= V4 192 32 32 255
  fillRect renderer (Just (Rectangle (P (V2 100 100)) (V2 20 20)))
  present renderer
  when (finished gs) (putStrLn "Done")
  return (finished gs)

isTimeUp :: SF GameState GameState
-- time :: SF a Time, arr 'lifts' a regular func to SF, >>> is SF bind
isTimeUp =
  Yampa.time >>> arr (\t ->
                       let isTime = t > 4
                       in GameState { finished = isTime })

initScreen :: IO Renderer
initScreen = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  return renderer


