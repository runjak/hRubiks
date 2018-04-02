module Strategy.CrossOnTop where

import Prelude hiding (Left, Right)
import Control.Monad

import Cube (Side(..))
import qualified Cube as Cube
import RotationPath (CubeMutation)
import Solve (solve)

{-|
  Bringing the edges of the top layer into their correct positions.

  The idea here is somewhat simple:
  1. Use Solve.solve to generate rotations to that bring the top front edge
     into the correct position for every possible starting position of that edge.
  2. Twist the cube to perform this operation for every edge of the top layer.
|-}

type Edge = ((Side, Int, Int), (Side, Int, Int))

edges :: [Edge]
edges = [
  -- Top to sides:
    ((Top, 0, 1), (Back, 0, 1))
  , ((Top, 1, 0), (Left, 0, 1))
  , ((Top, 1, 2), (Right, 0, 1))
  , ((Top, 2, 1), (Front, 0, 1))
  -- Sides to sides:
  , ((Front, 1, 2), (Right, 1, 0))
  , ((Right, 1, 2), (Back, 1, 0))
  , ((Back, 1, 2), (Left, 1, 0))
  , ((Left, 1, 2), (Front, 1, 0))
  -- Bottom to sides:
  , ((Bottom, 0, 1), (Front, 2 ,1))
  , ((Bottom, 1, 0), (Left, 2 ,1))
  , ((Bottom, 1, 2), (Right, 2 ,1))
  , ((Bottom, 2, 1), (Back, 2 ,1))
  ]

protectedEdges :: Edge -> Edge -> [Edge]
protectedEdges source target = do
  edge@(e1, e2) <- edges
  let edge' = (e2, e1)
  guard $ not $ edge == source || edge' == source
  guard $ not $ edge == target || edge' == target
  guard $ isTop e1 e2
  return edge
  where
    isTop (s1, _, _) (s2, _, _) = s1 == Top || s2 == Top
