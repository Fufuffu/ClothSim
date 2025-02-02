{-# OPTIONS -Wall #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when)
import qualified Data.Vector as V
import Raylib.Core (clearBackground, closeWindow, getFrameTime, getMousePosition, getMouseWheelMove, initWindow, isMouseButtonDown, setTargetFPS, windowShouldClose)
import Raylib.Core.Shapes (drawCircleV, drawLineV)
import Raylib.Types (Color (Color), MouseButton (MouseButtonLeft, MouseButtonRight), Vector2, pattern Vector2)
import Raylib.Util (WindowResources, drawing, raylibApplication)
import Raylib.Util.Colors (lightGray, rayWhite, red)
import Raylib.Util.Math

data Particle = Particle
  { currentPos :: Vector2,
    lastPos :: Vector2,
    isFixed :: Bool,
    isMarked :: Bool
  }
  deriving (Show, Eq)

data Constraint = Constraint
  { particle1Idx :: Int,
    particle2Idx :: Int,
    isActive :: Bool
  }
  deriving (Show, Eq)

data Cloth = Cloth
  { clothParticles :: V.Vector Particle,
    clothConstraints :: V.Vector Constraint
  }

data AppState = AppState
  { resources :: WindowResources,
    particles :: V.Vector Particle,
    constraints :: V.Vector Constraint,
    mouseSelectionRadius :: Float,
    mouseLastPos :: Vector2
  }

gravity :: Vector2
gravity = Vector2 0 490

particleSpacing :: Int
particleSpacing = 15

startup :: IO AppState
startup = do
  window <- initWindow 800 600 "Verlet integration"
  setTargetFPS 60

  let cloth = createCloth 600 400 particleSpacing 100 10
  return
    ( AppState
        { resources = window,
          particles = clothParticles cloth,
          constraints = clothConstraints cloth,
          mouseSelectionRadius = 30,
          mouseLastPos = Vector2 0 0
        }
    )

createCloth :: Int -> Int -> Int -> Int -> Int -> Cloth
createCloth pixelWidth pixelHeight spacing startX startY = do
  let width = div pixelWidth spacing
  let height = div pixelHeight spacing

  let horizontalConstraints = [Constraint (i - 1) i True | y <- [0 .. height], x <- [1 .. width], let i = y * (width + 1) + x]
  let verticalConstraints = [Constraint (i - (width + 1)) i True | y <- [1 .. height], x <- [0 .. width], let i = y * (width + 1) + x]

  let constraints = V.fromList (horizontalConstraints ++ verticalConstraints)
  let particles = V.generate ((width + 1) * (height + 1)) (createParticle spacing startX startY width)

  Cloth {clothParticles = particles, clothConstraints = constraints}

createParticle :: Int -> Int -> Int -> Int -> Int -> Particle
createParticle spacing startX startY width idx = do
  let (y, x) = divMod idx (width + 1)
  let pos = Vector2 (fromIntegral (startX + x * spacing)) (fromIntegral (startY + y * spacing))

  Particle {currentPos = pos, lastPos = pos, isFixed = y == 0, isMarked = False}

mainLoop :: AppState -> IO AppState
mainLoop state = do
  -- Since we do not run in background, dragging the window will break the simulation
  -- due to a huge delta time, this is ok for now :)
  dt <- getFrameTime
  mouseLeftDown <- isMouseButtonDown MouseButtonLeft
  mouseRightDown <- isMouseButtonDown MouseButtonRight
  mouseWheelDelta <- getMouseWheelMove
  mousePos <- getMousePosition
  drawing
    ( do
        clearBackground rayWhite

        drawCircleV mousePos (mouseSelectionRadius state) (Color 0 0 200 120)
        mapM_ (\(Particle pos _ _ _) -> drawCircleV pos 5 red) (particles state)
        mapM_ (drawConstraint (particles state)) (constraints state)
    )
    >> return
      state
        { particles =
            applyConstraints
              ( V.map
                  (updateParticle dt mousePos (mouseLastPos state) mouseLeftDown mouseRightDown (mouseSelectionRadius state))
                  (particles state)
              )
              (constraints state),
          constraints = V.map (updateConstraint (particles state)) (constraints state),
          mouseLastPos = mousePos,
          mouseSelectionRadius = mouseSelectionRadius state + mouseWheelDelta * 2
        }

updateConstraint :: V.Vector Particle -> Constraint -> Constraint
updateConstraint particles constraint =
  if isActive constraint
    then do
      let p1 = particles V.! particle1Idx constraint
      let p2 = particles V.! particle2Idx constraint

      constraint {isActive = not (isMarked p1 && isMarked p2)}
    else constraint

updateParticle :: Float -> Vector2 -> Vector2 -> Bool -> Bool -> Float -> Particle -> Particle
updateParticle dt mousePos mouseLastPos mouseLeftDown mouseRightDown selectionRadius particle =
  if isFixed particle
    then particle
    else do
      let nextPosition = (currentPos particle |* 2) |-| lastPos particle |+| (gravity |* (dt * dt))

      let distance = vectorDistance mousePos nextPosition
      let mouseDelta =
            if (distance < selectionRadius) && mouseLeftDown
              then vectorClamp (mousePos - mouseLastPos) (Vector2 (-30) (-30)) (Vector2 30 30)
              else Vector2 0 0

      particle
        { currentPos = vectorMin (nextPosition |+| mouseDelta) (Vector2 800 600),
          lastPos = currentPos particle |+| mouseDelta,
          isMarked = mouseRightDown && (distance < selectionRadius)
        }

applyConstraints :: V.Vector Particle -> V.Vector Constraint -> V.Vector Particle
applyConstraints = V.foldl' applyConstraint

applyConstraint :: V.Vector Particle -> Constraint -> V.Vector Particle
applyConstraint particles (Constraint idx1 idx2 isActive) = do
  let p1 = particles V.! idx1
  let p2 = particles V.! idx2

  let diff = currentPos p1 |-| currentPos p2
  let currentLength = magnitude diff
  let diffFactor = (fromIntegral particleSpacing - currentLength) / currentLength * 0.5
  let offset = diff |* diffFactor

  let newP1 =
        if not (isFixed p1) && isActive
          then p1 {currentPos = currentPos p1 |+| offset}
          else p1

  let newP2 =
        if not (isFixed p2) && isActive
          then p2 {currentPos = currentPos p2 |-| offset}
          else p2

  particles V.// [(idx1, newP1), (idx2, newP2)]

drawConstraint :: V.Vector Particle -> Constraint -> IO ()
drawConstraint particles (Constraint idx1 idx2 isActive) =
  when isActive $
    do
      let p1 = particles V.! idx1
      let p2 = particles V.! idx2
      drawLineV (currentPos p1) (currentPos p2) lightGray

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown state = closeWindow (Just (resources state))

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)