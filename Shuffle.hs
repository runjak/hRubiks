module Shuffle where

import Numeric.LinearAlgebra ((<>))
import qualified System.Random as Random

import qualified Cube as Cube
import Rotation (MatrixRotation)
import qualified Rotation as Rotation

shuffleRotations :: [MatrixRotation]
shuffleRotations = [
    Rotation.rightL
  , Rotation.rightR
  , Rotation.leftL
  , Rotation.leftR
  , Rotation.topL
  , Rotation.topR
  , Rotation.bottomL
  , Rotation.bottomR
  , Rotation.frontL
  , Rotation.frontR
  , Rotation.backL
  , Rotation.backR
  ]

randomRotations :: Int -> [MatrixRotation]
randomRotations seed =
  let gen = Random.mkStdGen seed
      indices = map (\x -> (abs x) `mod` length shuffleRotations) $ Random.randoms gen
  in map (shuffleRotations !!) indices

exampleShuffle =
  let rotations = take 20 $ randomRotations 5
      names = map Rotation.nameMatrixRotation rotations
      rotation = foldl (<>) Rotation.identity rotations
      cube = Rotation.rotate rotation Cube.solvedCube
  in (cube, names)
