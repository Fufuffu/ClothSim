{-# OPTIONS -Wall #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Vector as V
import Raylib.Core (clearBackground, closeWindow, getFrameTime, initWindow, setTargetFPS, windowShouldClose)
import Raylib.Core.Shapes (drawCircleV, drawLineV)
import Raylib.Core.Text (drawText)
import Raylib.Types (Vector2, pattern Vector2)
import Raylib.Util (WindowResources, drawing, raylibApplication)
import Raylib.Util.Colors (lightGray, rayWhite, red)
import Raylib.Util.Math

data Particle = Particle
  { currentPos :: Vector2,
    lastPos :: Vector2,
    isFixed :: Bool
  }
  deriving (Show, Eq)

data Constraint = Constraint
  { particle1Idx :: Int,
    particle2Idx :: Int
  }
  deriving (Show, Eq)

data Cloth = Cloth
  { clothParticles :: V.Vector Particle,
    clothConstraints :: V.Vector Constraint
  }

data AppState = AppState
  { resources :: WindowResources,
    particles :: V.Vector Particle,
    constraints :: V.Vector Constraint
  }

gravity :: Vector2
gravity = Vector2 0 98

particleSpacing :: Int
particleSpacing = 20

startup :: IO AppState
startup = do
  window <- initWindow 800 600 "Verlet integration"
  setTargetFPS 60

  let cloth = createCloth 600 400 particleSpacing 100 10
  return
    ( AppState
        { resources = window,
          particles = clothParticles cloth,
          constraints = clothConstraints cloth
        }
    )

createCloth :: Int -> Int -> Int -> Int -> Int -> Cloth
createCloth pixelWidth pixelHeight spacing startX startY = do
  let width = div pixelWidth spacing
  let height = div pixelHeight spacing

  let horizontalConstraints = [Constraint (i - 1) i | y <- [0 .. height], x <- [1 .. width], let i = y * (width + 1) + x]
  let verticalConstraints = [Constraint (i - (width + 1)) i | y <- [1 .. height], x <- [0 .. width], let i = y * (width + 1) + x]

  let constraints = V.fromList (horizontalConstraints ++ verticalConstraints)
  let particles = V.generate ((width + 1) * (height + 1)) (createParticle spacing startX startY width)

  Cloth {clothParticles = particles, clothConstraints = constraints}

createParticle :: Int -> Int -> Int -> Int -> Int -> Particle
createParticle spacing startX startY width idx = do
  let (y, x) = divMod idx (width + 1)
  let pos = Vector2 (fromIntegral (startX + x * spacing)) (fromIntegral (startY + y * spacing))

  Particle {currentPos = pos, lastPos = pos, isFixed = y == 0}

mainLoop :: AppState -> IO AppState
mainLoop state = do
  -- Since we do not run in background, dragging the window will break the simulation
  -- due to a huge delta time, this is ok for now :)
  dt <- getFrameTime
  drawing
    ( do
        clearBackground rayWhite

        mapM_ (\(Particle pos _ _) -> drawCircleV pos 5 red) (particles state)
        mapM_ (drawConstraint (particles state)) (constraints state)
    )
    >> return
      state
        { particles = applyConstraints (V.map (updateParticle dt) (particles state)) (constraints state)
        }

updateParticle :: Float -> Particle -> Particle
updateParticle dt particle =
  if isFixed particle
    then particle
    else do
      let nextPosition = (currentPos particle |* 2) |-| lastPos particle |+| (gravity |* (dt * dt))

      particle
        { currentPos = vectorMin nextPosition (Vector2 800 600),
          lastPos = currentPos particle
        }

applyConstraints :: V.Vector Particle -> V.Vector Constraint -> V.Vector Particle
applyConstraints particles constraints =
  V.foldl' applyConstraint particles constraints

applyConstraint :: V.Vector Particle -> Constraint -> V.Vector Particle
applyConstraint particles (Constraint idx1 idx2) = do
  let p1 = particles V.! idx1
  let p2 = particles V.! idx2

  let diff = currentPos p1 |-| currentPos p2
  let currentLength = magnitude diff
  let diffFactor = ((fromIntegral particleSpacing) - currentLength) / currentLength * 0.5
  let offset = diff |* diffFactor

  let newP1 =
        if not (isFixed p1)
          then p1 {currentPos = currentPos p1 |+| offset}
          else p1

  let newP2 =
        if not (isFixed p2)
          then p2 {currentPos = currentPos p2 |-| offset}
          else p2

  particles V.// [(idx1, newP1), (idx2, newP2)]

drawConstraint :: V.Vector Particle -> Constraint -> IO ()
drawConstraint particles (Constraint idx1 idx2) = do
  let p1 = particles V.! idx1
  let p2 = particles V.! idx2

  drawLineV (currentPos p1) (currentPos p2) lightGray

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown state = closeWindow (Just (resources state))

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)