module Cube where

import Numeric.LinearAlgebra (Z, Vector)
import qualified Data.List as List
import qualified Numeric.LinearAlgebra as LA

type SideField = (Int, Int)

fieldsOnSide :: [SideField]
fieldsOnSide = [(x,y)|x <- [0..2], y <- [0..2]]

data Side = Top | Front | Right | Back | Left | Bottom
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data Color = White | Green | Red | Blue | Orange | Yellow
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

type Cube = [(Side, Int, Int, Color)]

solvedCube :: Cube
solvedCube = do
  (side, color) <- zip [Top ..] [White ..]
  (x, y) <- fieldsOnSide
  [(side, x, y, color)]

testSolvedCube :: Bool
testSolvedCube =
  let sorted    = List.sort solvedCube == solvedCube
      allColors = (== [White ..])   . List.nub . List.sort $ fmap (\(_, _, _, c) -> c)      solvedCube
      allSides  = (== [Top ..])     . List.nub . List.sort $ fmap (\(s, _, _, _) -> s)      solvedCube
      allFields = (== fieldsOnSide) . List.nub . List.sort $ fmap (\(_, x, y, _) -> (x, y)) solvedCube
  in and [sorted, allColors, allSides, allFields]

cubeToVector :: Cube -> Vector Z
cubeToVector = LA.fromList . fmap go . List.sort
  where
    go :: (Side, Int, Int, Color) -> Z
    go (_, _, _, c) = fromIntegral $ fromEnum c

vectorToCube :: Vector Z -> Cube
vectorToCube = zipWith go solvedCube . fmap toColor . LA.toList
  where
    toColor :: Z -> Color
    toColor = toEnum . fromIntegral

    go :: (Side, Int, Int, Color) -> Color -> (Side, Int, Int, Color)
    go (s, x, y, _) c = (s, x, y, c)

testFromToVector :: Bool
testFromToVector =
  let solvedCube' = vectorToCube $ cubeToVector solvedCube
  in solvedCube' == solvedCube
