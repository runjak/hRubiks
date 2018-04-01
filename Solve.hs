module Solve where

import Control.Applicative ((<|>))
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Control.Monad.Writer as Writer

import Cube (Cube)
import qualified Cube as Cube
import Rotation (MatrixRotation)
import qualified Rotation as Rotation
import RotationPath (RotationPath)
import qualified RotationPath as RotationPath
import qualified Shuffle as Shuffle

type Predicate = Cube -> Bool

solve :: Int -> Predicate -> Cube -> Maybe (RotationPath Cube)
solve depth predicate cube
  | depth == 0 = Nothing
  | predicate cube = Just $ return cube
  | otherwise = bfs depth predicate $ map (RotationPath.rotate `flip` cube) Shuffle.shuffleRotations

bfs :: Int -> Predicate -> [RotationPath Cube] -> Maybe (RotationPath Cube)
bfs depth predicate cubes
  | depth == 0 = Nothing
  | otherwise =
    let (solved, unsolved) = List.partition (predicate . fst . Writer.runWriter) cubes
        nextRotations = [u >>= RotationPath.rotate r | u <- unsolved, r <- Shuffle.shuffleRotations]
    in Maybe.listToMaybe solved <|> bfs (depth - 1) predicate nextRotations
