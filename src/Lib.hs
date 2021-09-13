{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runGame
    ) where

import Control.Monad
import Data.IORef
import Data.Time.Clock
import FRP.Yampa as Yampa
import SDL as SDL

runGame :: IO ()
runGame = do
  renderer <- initScreen
  t <- getCurrentTime
  timeRef <- newIORef t
  reactimate initialise (input timeRef) (output renderer) isTimeUp

initialise :: IO Bool
initialise = do
  putStrLn "Starting..."
  return False

input :: IORef UTCTime -> Bool -> IO (DTime, Maybe Bool)
input ref _ = do
  currTime <- getCurrentTime
  lastTime <- readIORef ref
  writeIORef ref currTime
  let dt = diffUTCTime currTime lastTime
  return (realToFrac dt, Just True)

output :: Renderer -> Bool -> Bool -> IO Bool
output renderer _ x = do
  events <- pollEvents
  rendererDrawColor renderer $= V4 32 32 32 255
  clear renderer
  present renderer
  when x (putStrLn "Done")
  return x

isTimeUp :: SF Bool Bool
isTimeUp = Yampa.time >>> arr (> 4)

initScreen :: IO Renderer
initScreen = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  return renderer


