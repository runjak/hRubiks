module Strategy.Orientation where

import Prelude hiding (Right, Left)
import Numeric.LinearAlgebra ((<>))

import Cube (Color, Side(..), Cube)
import qualified Cube as Cube
import Rotation (MatrixRotation)
import qualified Rotation as Rotation
import RotationPath (CubeMutation)
import qualified RotationPath as RotationPath

{-|
  Orienting a cube so that other strategies have an oriented cube to work with.

  In the context of this module color of a side refers only to the middle field of a side.
|-}

sides :: Cube -> [(Side, Color)]
sides = map (\(s, _, _, c) -> (s, c)) . filter (\(_, x, y, _) -> x == 1 && y == 1)

isColorOnSide :: Color -> Side -> Cube -> Bool
isColorOnSide color side = any (\(s, c) -> (c == color && s == side)) . sides

-- | Returning (prefix, suffix) rotations to do something with a color on top
forColorOnTop :: Color -> Cube -> (MatrixRotation, MatrixRotation)
forColorOnTop color cube
  | isColorOnSide color Front cube = (Rotation.topToBack, Rotation.topToFront)
  | isColorOnSide color Right cube = (Rotation.topToLeft, Rotation.topToRight)
  | isColorOnSide color Back cube = (Rotation.topToFront, Rotation.topToBack)
  | isColorOnSide color Left cube = (Rotation.topToRight, Rotation.topToLeft)
  | isColorOnSide color Bottom cube =
    let r = Rotation.topToFront <> Rotation.topToFront
    in (r, r)
  | otherwise = (Rotation.identity, Rotation.identity)

putColorOnTop :: Color -> CubeMutation
putColorOnTop color cube =
  let (rotation, _) = forColorOnTop color cube
  in RotationPath.rotate rotation cube

withColorOnTop :: Color -> CubeMutation -> CubeMutation
withColorOnTop color λ cube =
  let (prefix, suffix) = forColorOnTop color cube
  in RotationPath.between prefix suffix λ cube
