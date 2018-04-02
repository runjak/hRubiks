module TestCube where

import Prelude hiding (Left, Right)

import Cube
import qualified Data.List as List

testSolvedCube :: Bool
testSolvedCube =
  let sorted    = List.sort solvedCube == solvedCube
      allColors = (== [White ..])   . List.nub . List.sort $ fmap (\(_, _, _, c) -> c)      solvedCube
      allSides  = (== [Top ..])     . List.nub . List.sort $ fmap (\(s, _, _, _) -> s)      solvedCube
      allFields = (== fieldsOnSide) . List.nub . List.sort $ fmap (\(_, x, y, _) -> (x, y)) solvedCube
  in and [sorted, allColors, allSides, allFields]

testComputeIndex :: Bool
testComputeIndex = fmap computeIndex solvedCube == [0..(length solvedCube - 1)]

testFromToVector :: Bool
testFromToVector =
  let solvedCube' = vectorToCube $ cubeToVector solvedCube
  in solvedCube' == solvedCube

testOppositeSides =
  let sidePairs = [(x, y) | x <- [Top ..], y <- [Top ..]]
      opposites = filter (uncurry oppositeSides) sidePairs
      expected = [
          (Top, Bottom)
        , (Front, Back)
        , (Right, Left)
        , (Back, Front)
        , (Left, Right)
        , (Bottom, Top)
        ]
  in opposites == expected

tests = [ ("solvedCube", testSolvedCube)
        , ("computeIndex", testComputeIndex)
        , ("fromToVector", testFromToVector)
        , ("oppositeSides", testOppositeSides)
        ]
