module TestRotation where

import Cube (Cube)
import Numeric.LinearAlgebra ((<>))
import Rotation
import qualified Cube as Cube
import qualified Data.List as List

testRotations :: MatrixRotation -> Bool
testRotations m =
  let ms = take 4 $ iterate (<> m) m
      areIdentity = fmap (== identity) ms
      testAreIdentity = areIdentity == [False, False, False, True]
      testAllDifferent = (4 ==) . length $ List.nub ms
  in testAreIdentity && testAllDifferent

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
  && testRotations topToFront

getSideColors :: Cube -> [(Cube.Side, Cube.Color)]
getSideColors = fmap head . filter (not . null) . List.group . fmap (\(s,_,_,c) -> (s,c))

testTopToBack :: Bool
testTopToBack = testRotations topToBack

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
  && testRotations topToRight

testTopToLeft :: Bool
testTopToLeft = testRotations topToLeft

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
  && testRotations topTwistRight

testTopTwistLeft :: Bool
testTopTwistLeft = testRotations topTwistLeft

testRightL :: Bool
testRightL =
  let [top, frn, rgt, bck, lft, bot] = [Cube.Top ..]
      [w, g, r, b, o, y] = [Cube.White ..]
      expected = [ (top, 0, 0, w), (top, 0, 1, w), (top, 0, 2, g), (top, 1, 0, w), (top, 1, 1, w), (top, 1, 2, g), (top, 2, 0, w), (top, 2, 1, w), (top, 2, 2, g)
                 , (frn, 0, 0, g), (frn, 0, 1, g), (frn, 0, 2, y), (frn, 1, 0, g), (frn, 1, 1, g), (frn, 1, 2, y), (frn, 2, 0, g), (frn, 2, 1, g), (frn, 2, 2, y)
                 , (rgt, 0, 0, r), (rgt, 0, 1, r), (rgt, 0, 2, r), (rgt, 1, 0, r), (rgt, 1, 1, r), (rgt, 1, 2, r), (rgt, 2, 0, r), (rgt, 2, 1, r), (rgt, 2, 2, r)
                 , (bck, 0, 0, w), (bck, 0, 1, b), (bck, 0, 2, b), (bck, 1, 0, w), (bck, 1, 1, b), (bck, 1, 2, b), (bck, 2, 0, w), (bck, 2, 1, b), (bck, 2, 2, b)
                 , (lft, 0, 0, o), (lft, 0, 1, o), (lft, 0, 2, o), (lft, 1, 0, o), (lft, 1, 1, o), (lft, 1, 2, o), (lft, 2, 0, o), (lft, 2, 1, o), (lft, 2, 2, o)
                 , (bot, 0, 0, y), (bot, 0, 1, y), (bot, 0, 2, b), (bot, 1, 0, y), (bot, 1, 1, y), (bot, 1, 2, b), (bot, 2, 0, y), (bot, 2, 1, y), (bot, 2, 2, b) ]
      rotated = toCubeRotation (toVectorRotation rightL) Cube.solvedCube
  in expected == rotated
  && testRotations rightL

testRightR :: Bool
testRightR = testRotations rightR

testLeftL =
  let [top, frn, rgt, bck, lft, bot] = [Cube.Top ..]
      [w, g, r, b, o, y] = [Cube.White ..]
      expected = [ (top, 0, 0, g), (top, 0, 1, w), (top, 0, 2, w), (top, 1, 0, g), (top, 1, 1, w), (top, 1, 2, w), (top, 2, 0, g), (top, 2, 1, w), (top, 2, 2, w)
                 , (frn, 0, 0, y), (frn, 0, 1, g), (frn, 0, 2, g), (frn, 1, 0, y), (frn, 1, 1, g), (frn, 1, 2, g), (frn, 2, 0, y), (frn, 2, 1, g), (frn, 2, 2, g)
                 , (rgt, 0, 0, r), (rgt, 0, 1, r), (rgt, 0, 2, r), (rgt, 1, 0, r), (rgt, 1, 1, r), (rgt, 1, 2, r), (rgt, 2, 0, r), (rgt, 2, 1, r), (rgt, 2, 2, r)
                 , (bck, 0, 0, b), (bck, 0, 1, b), (bck, 0, 2, w), (bck, 1, 0, b), (bck, 1, 1, b), (bck, 1, 2, w), (bck, 2, 0, b), (bck, 2, 1, b), (bck, 2, 2, w)
                 , (lft, 0, 0, o), (lft, 0, 1, o), (lft, 0, 2, o), (lft, 1, 0, o), (lft, 1, 1, o), (lft, 1, 2, o), (lft, 2, 0, o), (lft, 2, 1, o), (lft, 2, 2, o)
                 , (bot, 0, 0, b), (bot, 0, 1, y), (bot, 0, 2, y), (bot, 1, 0, b), (bot, 1, 1, y), (bot, 1, 2, y), (bot, 2, 0, b), (bot, 2, 1, y), (bot, 2, 2, y) ]
      rotated = toCubeRotation (toVectorRotation leftL) Cube.solvedCube
  in expected == rotated
  && testRotations leftL

testLeftR = testRotations leftR

tests = [ ("topToFront", testTopToFront)
        , ("topToBack", testTopToBack)
        , ("topToRight", testTopToRight)
        , ("topToLeft", testTopToLeft)
        , ("topTwistRight", testTopTwistRight)
        , ("topTwistLeft", testTopTwistLeft)
        , ("rightL", testRightL)
        , ("rightR", testRightR)
        , ("leftL", False)
        , ("leftR", testLeftR)
        ]
