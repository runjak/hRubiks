module Rotation where

import Control.Monad
import Numeric.LinearAlgebra ((<>), (#>), Z, Vector, Matrix)
import qualified Numeric.LinearAlgebra as LA
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Cube (Cube, cubeToVector, vectorToCube)
import qualified Cube as Cube

type CubeRotation = Cube -> Cube

toCubeRotation :: VectorRotation -> CubeRotation
toCubeRotation rotation = vectorToCube . rotation . cubeToVector

type VectorRotation = Vector Z -> Vector Z

toVectorRotation :: MatrixRotation -> VectorRotation
toVectorRotation = (#>)

rotate :: MatrixRotation -> Cube -> Cube
rotate rotation = toCubeRotation $ toVectorRotation rotation

type MatrixRotation = Matrix Z

showMatrixRotation :: MatrixRotation -> String
showMatrixRotation = show . fmap (go 0 . LA.toList) . LA.toColumns
  where
    go i [] = i
    go i (v:vs)
      | v == 1 = i
      | otherwise = go (i + 1) vs

identity :: MatrixRotation
identity = LA.ident $ 9 * 6

topToFront :: MatrixRotation
topToFront =
  let vs = LA.toColumns identity
      mapping = [  9, 10, 11, 12, 13, 14, 15, 16, 17
                , 45, 46, 47, 48, 49, 50, 51, 52, 53
                , 24, 21, 18, 25, 22, 19, 26, 23, 20
                ,  8,  7,  6,  5,  4,  3,  2,  1,  0
                , 38, 41, 44, 37, 40, 43, 36, 39, 42
                , 35, 34, 33, 32, 31, 30, 29, 28, 27 ]
  in LA.fromColumns $ fmap (vs !!) mapping

topToBack :: MatrixRotation
topToBack = topToFront <> topToFront <> topToFront

topToRight :: MatrixRotation
topToRight =
  let vs = LA.toColumns identity
      mapping = [ 20, 23, 26, 19, 22, 25, 18, 21, 24
                , 11, 14, 17, 10, 13, 16,  9, 12, 15
                , 47, 50, 53, 46, 49, 52, 45, 48, 51
                , 33, 30, 27, 34, 31, 28, 35, 32, 29
                ,  2,  5,  8,  1,  4,  7,  0,  3,  6
                , 38, 41, 44, 37, 40, 43, 36, 39, 42 ]
  in LA.fromColumns $ fmap (vs !!) mapping

topToLeft :: MatrixRotation
topToLeft = topToRight <> topToRight <> topToRight

topTwistRight :: MatrixRotation
topTwistRight = topToBack <> topToRight <> topToFront

topTwistLeft :: MatrixRotation
topTwistLeft = topTwistRight <> topTwistRight <> topTwistRight

rightL :: MatrixRotation
rightL =
  let vs = LA.toColumns identity
      mapping = [  0,  1, 11,  3,  4, 14,  6,  7, 17
                ,  9, 10, 47, 12, 13, 50, 15, 16, 53
                , 24, 21, 18, 25, 22, 19, 26, 23, 20
                ,  8, 28, 29,  5, 31, 32,  2, 34, 35
                , 36, 37, 38, 39, 40, 41, 42, 43, 44
                , 45, 46, 33, 48, 49, 30, 51, 52, 27 ]
  in LA.fromColumns $ fmap (vs !!) mapping

rightR :: MatrixRotation
rightR = rightL <> rightL <> rightL

leftL :: MatrixRotation
leftL =
  let twist = topTwistRight <> topTwistRight
  in twist <> rightL <> twist

leftR :: MatrixRotation
leftR = leftL <> leftL <> leftL

topL :: MatrixRotation
topL = topToLeft <> rightL <> topToRight

topR :: MatrixRotation
topR = topL <> topL <> topL

bottomL :: MatrixRotation
bottomL =
  let twist = topToRight <> topToRight
  in twist <> topL <> twist

bottomR :: MatrixRotation
bottomR = bottomL <> bottomL <> bottomL

frontL :: MatrixRotation
frontL = topTwistRight <> rightL <> topTwistLeft

frontR :: MatrixRotation
frontR = frontL <> frontL <> frontL

backL :: MatrixRotation
backL =
  let twist = topTwistLeft <> topTwistLeft
  in twist <> frontL <> twist

backR :: MatrixRotation
backR = backL <> backL <> backL

rotations :: [(MatrixRotation, String)]
rotations = [
    (identity, "identity")
  , (topToFront, "topToFront")
  , (topToBack, "topToBack")
  , (topToRight, "topToRight")
  , (topToLeft, "topToLeft")
  , (topTwistRight, "topTwistRight")
  , (topTwistLeft, "topTwistLeft")
  , (rightL, "rightL")
  , (rightR, "rightR")
  , (leftL, "leftL")
  , (leftR, "leftR")
  , (topL, "topL")
  , (topR, "topR")
  , (bottomL, "bottomL")
  , (bottomR, "bottomR")
  , (frontL, "frontL")
  , (frontR, "frontR")
  , (backL, "backL")
  , (backR, "backR")
  ]

nameMatrixRotation :: MatrixRotation -> String
nameMatrixRotation = Maybe.fromMaybe "unknown" . flip List.lookup rotations

inverse :: MatrixRotation -> Maybe MatrixRotation
inverse r = Maybe.listToMaybe $ do
  r' <- map fst rotations
  guard $ r <> r' == identity
  return r'
