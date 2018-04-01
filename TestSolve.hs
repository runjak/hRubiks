module TestSolve where

import qualified Data.DList as DList
import qualified Data.Maybe as Maybe
import qualified Control.Monad.Writer as Writer

import qualified Cube as Cube
import qualified Rotation as Rotation
import Solve (solve)

testSolve =
  let unsolvedCube = foldl (flip Rotation.rotate) Cube.solvedCube [Rotation.rightR, Rotation.frontL]
      (Just (cube', history)) = fmap Writer.runWriter $ solve 2 (== Cube.solvedCube) unsolvedCube
      isSolved = cube' == Cube.solvedCube
      correctHistory = DList.toList history == [Rotation.frontR, Rotation.rightL]
      isUnsolved = Maybe.isNothing $ solve 1 (== Cube.solvedCube) unsolvedCube
  in isSolved && correctHistory && isUnsolved

tests :: [(String, Bool)]
tests = [
    ("Solve.solve", testSolve)
  ]
