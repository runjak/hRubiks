module Cube where

import Prelude hiding (Left, Right)
import Data.Function (on)
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

computeIndex :: (Side, Int, Int, Color) -> Int
computeIndex (_, x, y, c) = x * 3 + y + fromEnum c * 9

cubeToVector :: Cube -> Vector Z
cubeToVector = LA.fromList . fmap go . List.sortBy (compare `on` project)
  where
    project (a,b,c,_) = (a,b,c)

    go :: (Side, Int, Int, Color) -> Z
    go (_, _, _, c) = fromIntegral $ fromEnum c

vectorToCube :: Vector Z -> Cube
vectorToCube = zipWith go solvedCube . fmap toColor . LA.toList
  where
    toColor :: Z -> Color
    toColor = toEnum . fromIntegral

    go :: (Side, Int, Int, Color) -> Color -> (Side, Int, Int, Color)
    go (s, x, y, _) c = (s, x, y, c)

oppositeSides :: Side -> Side -> Bool
oppositeSides Top Bottom = True
oppositeSides Front Back = True
oppositeSides Right Left = True
oppositeSides x y
  | fromEnum x > fromEnum y = oppositeSides y x
  | otherwise = False
