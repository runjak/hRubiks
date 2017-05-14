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

dim :: (Int, Int)
dim = let d = 9*6 in (d, d)

identity :: Matrix Z
identity = LA.ident $ fst dim

topToFront :: MatrixRotation
topToFront =
  let vs = LA.toColumns identity
      top    = take 9 vs
      front  = take 9 $ drop 9 vs
      {-
        Index mapping for right:
        [ 18, 19, 20     [ 20, 23, 26
        , 21, 22, 23  -> , 19, 22, 25
        , 24, 25, 26 ]   , 18, 21, 24 ]
      -}
      right' = fmap (vs !!) [20, 23, 26, 19, 22, 25, 18, 21, 24]
      back   = take 9 $ drop 27 vs
      {-
        Index mapping for left:
        [ 36, 37, 38     [ 38, 41, 44
        , 39, 40, 41  -> , 37, 40, 43
        , 42, 43, 44 ]   , 36, 39, 42 ]
      -}
      left'  = fmap (vs !!) [38, 41, 44, 37, 40, 43, 36, 39, 42]
      bottom = drop 45 vs
  in LA.fromColumns $ concat [front, bottom, right', top, left', back]

topToBack :: MatrixRotation
topToBack = topToFront <> topToFront <> topToFront

topToRight :: MatrixRotation
topToRight =
  let vs = LA.toColumns identity
      top = take 9 vs
      {-
        Index mapping for front:
        [  9, 10, 11     [ 15, 12, 9
        , 12, 13, 14  -> , 16, 13, 10
        , 15, 16, 17 ]   , 17, 14, 11 ]
      -}
      front' = fmap (vs !!) [15, 12, 9, 16, 13, 10, 17, 14, 11]
      right = take 9 $ drop 18 vs
      {-
        Index mapping for back:
        [ 27, 28, 29     [ 29, 32, 35
        , 30, 31, 32  -> , 28, 31, 34
        , 33, 34, 35 ]   , 27, 30, 33 ]
      -}
      back' = fmap (vs !!) [29, 32, 35, 28, 31, 34, 27, 30, 33]
      left = take 9 $ drop 36 vs
      bottom = drop 45 vs
  in LA.fromColumns $ concat [right, front', bottom, back', top, left]

topToLeft :: MatrixRotation
topToLeft = topToRight <> topToRight <> topToRight

topTwistRight :: MatrixRotation
topTwistRight = topToBack <> topToRight <> topToFront

topTwistLeft :: MatrixRotation
topTwistLeft = topTwistRight <> topTwistRight <> topTwistRight

rightL :: MatrixRotation
rightL =
  let vs = LA.toColumns identity
      top    = fmap (vs !!) [ 0,  1, 33,  3,  4, 30,  6,  7, 27]
      front  = fmap (vs !!) [ 9, 10,  2, 12, 13,  5, 15, 16,  8]
      back   = fmap (vs !!) [53, 28, 29, 50, 31, 32, 47, 34, 35]
      bottom = fmap (vs !!) [45, 46, 11, 48, 49, 14, 51, 52, 17]
      {-
        Index mapping for right:
        [ 18, 19, 20     [ 20, 23, 26
        , 21, 22, 23  -> , 19, 22, 25
        , 24, 25, 26 ]   , 18, 21, 24 ]
      -}
      right = fmap (vs !!) [20, 23, 26, 19, 22, 25, 18, 21, 24]
      left = take 9 $ drop 36 vs
  in LA.fromColumns $ concat [top, front, right, back, left, bottom]

rightR :: MatrixRotation
rightR = rightL <> rightL <> rightL

leftL :: MatrixRotation
leftL =
  let twist = topTwistRight <> topTwistRight
  in twist <> rightL <> twist

{-
  [  9,  1,  2, 12,  4,  5, 15,  7,  8
  , 45, 10, 11, 48, 13, 14, 51, 16, 17
  , 18, 19, 20, 21, 22, 23, 24, 25, 26
  , 27, 28,  6, 30, 31,  3, 33, 34,  0
  , 38, 41, 44, 37, 40, 43, 36, 39, 42
  , 35, 46, 47, 32, 49, 50, 29, 52, 53 ]
-}
leftL' :: MatrixRotation
leftL' =
  let vs = LA.toColumns identity
      cols = [  9,  1,  2, 12,  4,  5, 15,  7,  8
             , 45, 10, 11, 48, 13, 14, 51, 16, 17
             , 18, 19, 20, 21, 22, 23, 24, 25, 26
             , 27, 28,  6, 30, 31,  3, 33, 34,  0
             , 38, 41, 44, 37, 40, 43, 36, 39, 42
             , 35, 46, 47, 32, 49, 50, 29, 52, 53 ]
  in LA.fromColumns $ fmap (vs !!) cols

leftR :: MatrixRotation
leftR = leftL <> leftL <> leftL

upL :: MatrixRotation
upL = undefined

upR :: MatrixRotation
upR = undefined

downL :: MatrixRotation
downL = undefined

downR :: MatrixRotation
downR = undefined

frontL :: MatrixRotation
frontL = undefined

frontR :: MatrixRotation
frontR = undefined

backL :: MatrixRotation
backL = undefined

backR :: MatrixRotation
backR = undefined
