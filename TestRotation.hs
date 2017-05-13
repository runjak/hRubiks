module TestRotation where

import Cube (Cube)
import Rotation
import qualified Cube as Cube
import qualified Data.List as List

testTopToFront :: Bool
testTopToFront =
  let expected = [ (Cube.Top, Cube.Blue)
                 , (Cube.Front, Cube.White)
                 , (Cube.Right, Cube.Red)
                 , (Cube.Back, Cube.Yellow)
                 , (Cube.Left, Cube.Orange)
                 , (Cube.Bottom, Cube.Green) ]
      rotated = toCubeRotation (toVectorRotation topToFront) Cube.solvedCube
  in expected == getSideColors rotated
  && identity == rotateIdentity topToFront

getSideColors :: Cube -> [(Cube.Side, Cube.Color)]
getSideColors = fmap head . filter (not . null) . List.group . fmap (\(s,_,_,c) -> (s,c))

testTopToBack :: Bool
testTopToBack = identity == rotateIdentity topToBack

testTopToRight :: Bool
testTopToRight =
  let expected = [ (Cube.Top, Cube.Orange)
                 , (Cube.Front, Cube.Green)
                 , (Cube.Right, Cube.White)
                 , (Cube.Back, Cube.Blue)
                 , (Cube.Left, Cube.Yellow)
                 , (Cube.Bottom, Cube.Red) ]
      rotated = toCubeRotation (toVectorRotation topToRight) Cube.solvedCube
  in expected == getSideColors rotated
  && identity == rotateIdentity topToRight

testTopToLeft :: Bool
testTopToLeft = identity == rotateIdentity topToLeft

testTopTwistRight :: Bool
testTopTwistRight =
  let expected = [ (Cube.Top, Cube.White)
                 , (Cube.Front, Cube.Red)
                 , (Cube.Right, Cube.Blue)
                 , (Cube.Back, Cube.Orange)
                 , (Cube.Left, Cube.Green)
                 , (Cube.Bottom, Cube.Yellow) ]
      rotated = toCubeRotation (toVectorRotation topTwistRight) Cube.solvedCube
  in expected == getSideColors rotated
  && identity == rotateIdentity topTwistRight

testTopTwistLeft :: Bool
testTopTwistLeft = identity == rotateIdentity topTwistLeft

tests = [ ("topToFront", testTopToFront)
        , ("topToBack", testTopToBack)
        , ("topToRight", testTopToRight)
        , ("topToLeft", testTopToLeft)
        , ("topTwistRight", testTopTwistRight)
        , ("topTwistLeft", testTopTwistLeft)
        ]
