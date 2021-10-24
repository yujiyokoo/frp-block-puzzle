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
  ((replicate 22 $ ([True] ++ (replicate 10 False) ++ [True]))) ++ [(replicate 12 True)]

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
    (Yampa.identity &&& Yampa.constant False) >>> (setBlockPosition rg initialState)

buildGameState :: GameState -> (PlacedBlock, Bool) -> GameState
buildGameState initialState (block, f) =
  initialState { finished = f
     , currentBlock = block
  }

placeBlock :: BlockPosition -> GameState -> GameState
placeBlock (x, y) gs =
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



setBlockPosition :: RandomGen rg => rg -> GameState -> SF ([SDL.Event], Bool) GameState
setBlockPosition rg gs = switch (sf >>> second notYet) cont
  where sf = proc (events, t) -> do
          let buttonPresses = buttonPressesFrom events
              block = currentBlock gs
              (x, y) = position $ block
          dy <- integral -< (1.0 :: Float)
          newY <- arr (\(a, b) -> (a + b)) -< (y, dy)
          hasQuit <- arr quitKey -< buttonPresses
          newGameState <- arr (buildGameState gs) -< (block { position = (x, newY) }, hasQuit)
          now <- localTime -< ()
          moveEvent <- arr moveBlock -< (rg, buttonPresses, block { position = (x, newY) }, (playFieldState gs))
          returnA -< (newGameState, moveEvent `attach` hasQuit)
        cont ((block@(PlacedBlock { position = (x, y)}), keepBlocksAt, newRg), q) =
          let
            newGameState = buildGameState gs (block, q)
          in
          case keepBlocksAt of
            [] ->
              setBlockPosition newRg newGameState
            positions ->
              pause gs (Yampa.localTime >>^ (< 1.0)) (setBlockPosition newRg (foldr placeBlock newGameState positions))

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


