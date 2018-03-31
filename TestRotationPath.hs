module TestRotationPath where

import qualified Control.Monad.Writer as Writer
import qualified Data.DList as DList

import qualified Cube as Cube
import qualified Rotation as Rotation
import qualified RotationPath as RotationPath

testRotate :: Bool
testRotate =
  let testRotations = [
          Rotation.topToFront
        , Rotation.frontR
        , Rotation.topToBack
        ]
      (cube', rotationLog) = Writer.runWriter $
        Writer.foldM (flip RotationPath.rotate) Cube.solvedCube testRotations
      expectedCube = Rotation.rotate Rotation.topR Cube.solvedCube
  in cube' == expectedCube && testRotations == DList.toList rotationLog

testBetween :: Bool
testBetween =
  let expectedCube = Rotation.rotate Rotation.topR Cube.solvedCube
      (cube', rotationLog) = Writer.runWriter $
        RotationPath.between Rotation.topToFront Rotation.topToBack
          (RotationPath.rotate Rotation.frontR) Cube.solvedCube
  in cube' == expectedCube && [Rotation.topR] == DList.toList rotationLog

tests :: [(String, Bool)]
tests = [
    ("RotationPath.rotate", testRotate)
  , ("RotationPath.between", testBetween)
  ]
