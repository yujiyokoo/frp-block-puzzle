module Main where

import Lib
import System.Random (newStdGen)

main :: IO ()
main = do
  sg <- newStdGen
  runGame sg
