module Rotation where

import Cube (Cube, cubeToVector, vectorToCube)
import Numeric.LinearAlgebra ((<>), (#>), Z, Vector, Matrix)
import qualified Cube as Cube
import qualified Numeric.LinearAlgebra as LA

type CubeRotation = Cube -> Cube

toCubeRotation :: VectorRotation -> CubeRotation
toCubeRotation rotation = vectorToCube . rotation . cubeToVector

type VectorRotation = Vector Z -> Vector Z

toVectorRotation :: MatrixRotation -> VectorRotation
toVectorRotation = (#>)

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
      mapping = [  0,  1, 33,  3,  4, 30,  6,  7, 27
                ,  9, 10,  2, 12, 13,  5, 15, 16,  8
                , 20, 23, 26, 19, 22, 25, 18, 21, 24
                , 53, 28, 29, 50, 31, 32, 47, 34, 35
                , 36, 37, 38, 39, 40, 41, 42, 43, 44
                , 45, 46, 11, 48, 49, 14, 51, 52, 17 ]
  in LA.fromColumns $ fmap (vs !!) mapping

rightR :: MatrixRotation
rightR = rightL <> rightL <> rightL

leftL :: MatrixRotation
leftL =
  let twist = topTwistRight <> topTwistRight
  in twist <> rightR <> twist

leftR :: MatrixRotation
leftR = leftL <> leftL <> leftL

topL :: MatrixRotation
topL = topToLeft <> rightR <> topToRight

topR :: MatrixRotation
topR = topL <> topL <> topL

bottomL :: MatrixRotation
bottomL = undefined

bottomR :: MatrixRotation
bottomR = bottomL <> bottomL <> bottomL

frontL :: MatrixRotation
frontL = undefined

frontR :: MatrixRotation
frontR = frontL <> frontL <> frontL

backL :: MatrixRotation
backL = undefined

backR :: MatrixRotation
backR = backL <> backL <> backL
