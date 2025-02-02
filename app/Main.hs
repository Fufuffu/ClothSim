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
    mass :: Float
  }
  deriving (Show, Eq)

data Constraint = Constraint
  { particle1Idx :: Int,
    particle2Idx :: Int,
    length :: Float
  }
  deriving (Show, Eq)

data AppState = AppState
  { resources :: WindowResources,
    particles :: V.Vector Particle,
    constraints :: V.Vector Constraint
  }

startup :: IO AppState
startup = do
  window <- initWindow 800 600 "Verlet integration"
  setTargetFPS 60
  let initialParticles =
        V.fromList
          [ Particle (Vector2 20 20) (Vector2 20 20) 1,
            Particle (Vector2 80 20) (Vector2 80 20) 1,
            Particle (Vector2 80 60) (Vector2 80 60) 1,
            Particle (Vector2 20 60) (Vector2 20 60) 1
          ]
  return
    ( AppState
        { resources = window,
          particles = initialParticles,
          constraints =
            V.fromList
              [ createConstraint initialParticles 0 1,
                createConstraint initialParticles 1 2,
                createConstraint initialParticles 2 3,
                createConstraint initialParticles 3 0
              ]
        }
    )

createConstraint :: V.Vector Particle -> Int -> Int -> Constraint
createConstraint particleVector idx1 idx2 =
  Constraint idx1 idx2 (vectorDistance (currentPos (particleVector V.! idx1)) (currentPos (particleVector V.! idx2)))

mainLoop :: AppState -> IO AppState
mainLoop state = do
  dt <- getFrameTime
  drawing
    ( do
        clearBackground rayWhite
        drawText "Basic raylib window" 20 40 18 lightGray
        mapM_ (\(Particle pos _ _) -> drawCircleV pos 5 red) (particles state)
        mapM_ (drawConstraint (particles state)) (constraints state)
    )
    >> return
      state
        { particles = applyConstraints (V.map (updateParticle dt) (particles state)) (constraints state)
        }

drawConstraint :: V.Vector Particle -> Constraint -> IO ()
drawConstraint particles (Constraint idx1 idx2 _) = do
  let p1 = particles V.! idx1
  let p2 = particles V.! idx2

  drawLineV (currentPos p1) (currentPos p2) lightGray

updateParticle :: Float -> Particle -> Particle
updateParticle dt particle = do
  let force = Vector2 0 10
  let acceleration = force |/ mass particle

  let nextPosition = (currentPos particle |* 2) |-| lastPos particle |+| (acceleration |* (dt * dt))

  particle
    { currentPos = vectorMin nextPosition (Vector2 800 600),
      lastPos = currentPos particle
    }

applyConstraints :: V.Vector Particle -> V.Vector Constraint -> V.Vector Particle
applyConstraints particles constraints =
  V.foldl' applyConstraint particles constraints

applyConstraint :: V.Vector Particle -> Constraint -> V.Vector Particle
applyConstraint particles (Constraint idx1 idx2 consLength) = do
  let p1 = particles V.! idx1
  let p2 = particles V.! idx2

  let diff = currentPos p1 |-| currentPos p2
  let currentLength = magnitude diff
  let diffFactor = (consLength - currentLength) / currentLength * 0.5
  let offset = diff |* diffFactor

  let newP1 = p1 {currentPos = currentPos p1 |+| offset}
  let newP2 = p2 {currentPos = currentPos p2 |-| offset}

  particles V.// [(idx1, newP1), (idx2, newP2)]

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown state = closeWindow (Just (resources state))

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)