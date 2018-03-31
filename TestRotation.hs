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
  let [top, frn, rgt, bck, lft, bot] = [Cube.Top ..]
      [w, g, r, b, o, y] = [Cube.White ..]
      expected = [ (top, 0, 0, b), (top, 0, 1, b), (top, 0, 2, b), (top, 1, 0, b), (top, 1, 1, b), (top, 1, 2, b), (top, 2, 0, b), (top, 2, 1, b), (top, 2, 2, b)
                 , (frn, 0, 0, w), (frn, 0, 1, w), (frn, 0, 2, w), (frn, 1, 0, w), (frn, 1, 1, w), (frn, 1, 2, w), (frn, 2, 0, w), (frn, 2, 1, w), (frn, 2, 2, w)
                 , (rgt, 0, 0, r), (rgt, 0, 1, r), (rgt, 0, 2, r), (rgt, 1, 0, r), (rgt, 1, 1, r), (rgt, 1, 2, r), (rgt, 2, 0, r), (rgt, 2, 1, r), (rgt, 2, 2, r)
                 , (bck, 0, 0, y), (bck, 0, 1, y), (bck, 0, 2, y), (bck, 1, 0, y), (bck, 1, 1, y), (bck, 1, 2, y), (bck, 2, 0, y), (bck, 2, 1, y), (bck, 2, 2, y)
                 , (lft, 0, 0, o), (lft, 0, 1, o), (lft, 0, 2, o), (lft, 1, 0, o), (lft, 1, 1, o), (lft, 1, 2, o), (lft, 2, 0, o), (lft, 2, 1, o), (lft, 2, 2, o)
                 , (bot, 0, 0, g), (bot, 0, 1, g), (bot, 0, 2, g), (bot, 1, 0, g), (bot, 1, 1, g), (bot, 1, 2, g), (bot, 2, 0, g), (bot, 2, 1, g), (bot, 2, 2, g) ]
      rotated = rotate topToFront Cube.solvedCube
  in expected == rotated
  && testRotations topToFront

getSideColors :: Cube -> [(Cube.Side, Cube.Color)]
getSideColors = fmap head . filter (not . null) . List.group . fmap (\(s,_,_,c) -> (s,c))

testTopToBack :: Bool
testTopToBack = testRotations topToBack

testTopToRight :: Bool
testTopToRight =
  let [top, frn, rgt, bck, lft, bot] = [Cube.Top ..]
      [w, g, r, b, o, y] = [Cube.White ..]
      expected = [ (top, 0, 0, o), (top, 0, 1, o), (top, 0, 2, o), (top, 1, 0, o), (top, 1, 1, o), (top, 1, 2, o), (top, 2, 0, o), (top, 2, 1, o), (top, 2, 2, o)
                 , (frn, 0, 0, g), (frn, 0, 1, g), (frn, 0, 2, g), (frn, 1, 0, g), (frn, 1, 1, g), (frn, 1, 2, g), (frn, 2, 0, g), (frn, 2, 1, g), (frn, 2, 2, g)
                 , (rgt, 0, 0, w), (rgt, 0, 1, w), (rgt, 0, 2, w), (rgt, 1, 0, w), (rgt, 1, 1, w), (rgt, 1, 2, w), (rgt, 2, 0, w), (rgt, 2, 1, w), (rgt, 2, 2, w)
                 , (bck, 0, 0, b), (bck, 0, 1, b), (bck, 0, 2, b), (bck, 1, 0, b), (bck, 1, 1, b), (bck, 1, 2, b), (bck, 2, 0, b), (bck, 2, 1, b), (bck, 2, 2, b)
                 , (lft, 0, 0, y), (lft, 0, 1, y), (lft, 0, 2, y), (lft, 1, 0, y), (lft, 1, 1, y), (lft, 1, 2, y), (lft, 2, 0, y), (lft, 2, 1, y), (lft, 2, 2, y)
                 , (bot, 0, 0, r), (bot, 0, 1, r), (bot, 0, 2, r), (bot, 1, 0, r), (bot, 1, 1, r), (bot, 1, 2, r), (bot, 2, 0, r), (bot, 2, 1, r), (bot, 2, 2, r) ]
      rotated = rotate topToRight Cube.solvedCube
  in expected == rotated
  && testRotations topToRight

testTopToLeft :: Bool
testTopToLeft = testRotations topToLeft

testTopTwistRight :: Bool
testTopTwistRight =
  let [top, frn, rgt, bck, lft, bot] = [Cube.Top ..]
      [w, g, r, b, o, y] = [Cube.White ..]
      expected = [ (top, 0, 0, w), (top, 0, 1, w), (top, 0, 2, w), (top, 1, 0, w), (top, 1, 1, w), (top, 1, 2, w), (top, 2, 0, w), (top, 2, 1, w), (top, 2, 2, w)
                 , (frn, 0, 0, r), (frn, 0, 1, r), (frn, 0, 2, r), (frn, 1, 0, r), (frn, 1, 1, r), (frn, 1, 2, r), (frn, 2, 0, r), (frn, 2, 1, r), (frn, 2, 2, r)
                 , (rgt, 0, 0, b), (rgt, 0, 1, b), (rgt, 0, 2, b), (rgt, 1, 0, b), (rgt, 1, 1, b), (rgt, 1, 2, b), (rgt, 2, 0, b), (rgt, 2, 1, b), (rgt, 2, 2, b)
                 , (bck, 0, 0, o), (bck, 0, 1, o), (bck, 0, 2, o), (bck, 1, 0, o), (bck, 1, 1, o), (bck, 1, 2, o), (bck, 2, 0, o), (bck, 2, 1, o), (bck, 2, 2, o)
                 , (lft, 0, 0, g), (lft, 0, 1, g), (lft, 0, 2, g), (lft, 1, 0, g), (lft, 1, 1, g), (lft, 1, 2, g), (lft, 2, 0, g), (lft, 2, 1, g), (lft, 2, 2, g)
                 , (bot, 0, 0, y), (bot, 0, 1, y), (bot, 0, 2, y), (bot, 1, 0, y), (bot, 1, 1, y), (bot, 1, 2, y), (bot, 2, 0, y), (bot, 2, 1, y), (bot, 2, 2, y) ]
      rotated = rotate topTwistRight Cube.solvedCube
  in expected == rotated
  && testRotations topTwistRight

testTopTwistLeft :: Bool
testTopTwistLeft = testRotations topTwistLeft

testRightL :: Bool
testRightL =
  let [top, frn, rgt, bck, lft, bot] = [Cube.Top ..]
      [w, g, r, b, o, y] = [Cube.White ..]
      expected = [ (top, 0, 0, w), (top, 0, 1, w), (top, 0, 2, b), (top, 1, 0, w), (top, 1, 1, w), (top, 1, 2, b), (top, 2, 0, w), (top, 2, 1, w), (top, 2, 2, b)
                 , (frn, 0, 0, g), (frn, 0, 1, g), (frn, 0, 2, w), (frn, 1, 0, g), (frn, 1, 1, g), (frn, 1, 2, w), (frn, 2, 0, g), (frn, 2, 1, g), (frn, 2, 2, w)
                 , (rgt, 0, 0, r), (rgt, 0, 1, r), (rgt, 0, 2, r), (rgt, 1, 0, r), (rgt, 1, 1, r), (rgt, 1, 2, r), (rgt, 2, 0, r), (rgt, 2, 1, r), (rgt, 2, 2, r)
                 , (bck, 0, 0, y), (bck, 0, 1, b), (bck, 0, 2, b), (bck, 1, 0, y), (bck, 1, 1, b), (bck, 1, 2, b), (bck, 2, 0, y), (bck, 2, 1, b), (bck, 2, 2, b)
                 , (lft, 0, 0, o), (lft, 0, 1, o), (lft, 0, 2, o), (lft, 1, 0, o), (lft, 1, 1, o), (lft, 1, 2, o), (lft, 2, 0, o), (lft, 2, 1, o), (lft, 2, 2, o)
                 , (bot, 0, 0, y), (bot, 0, 1, y), (bot, 0, 2, g), (bot, 1, 0, y), (bot, 1, 1, y), (bot, 1, 2, g), (bot, 2, 0, y), (bot, 2, 1, y), (bot, 2, 2, g) ]
      rotated = rotate rightL Cube.solvedCube
  in expected == rotated
  && testRotations rightL

testRightR :: Bool
testRightR = testRotations rightR

testLeftL :: Bool
testLeftL =
  let [top, frn, rgt, bck, lft, bot] = [Cube.Top ..]
      [w, g, r, b, o, y] = [Cube.White ..]
      expected = [ (top, 0, 0, g), (top, 0, 1, w), (top, 0, 2, w), (top, 1, 0, g), (top, 1, 1, w), (top, 1, 2, w), (top, 2, 0, g), (top, 2, 1, w), (top, 2, 2, w)
                 , (frn, 0, 0, y), (frn, 0, 1, g), (frn, 0, 2, g), (frn, 1, 0, y), (frn, 1, 1, g), (frn, 1, 2, g), (frn, 2, 0, y), (frn, 2, 1, g), (frn, 2, 2, g)
                 , (rgt, 0, 0, r), (rgt, 0, 1, r), (rgt, 0, 2, r), (rgt, 1, 0, r), (rgt, 1, 1, r), (rgt, 1, 2, r), (rgt, 2, 0, r), (rgt, 2, 1, r), (rgt, 2, 2, r)
                 , (bck, 0, 0, b), (bck, 0, 1, b), (bck, 0, 2, w), (bck, 1, 0, b), (bck, 1, 1, b), (bck, 1, 2, w), (bck, 2, 0, b), (bck, 2, 1, b), (bck, 2, 2, w)
                 , (lft, 0, 0, o), (lft, 0, 1, o), (lft, 0, 2, o), (lft, 1, 0, o), (lft, 1, 1, o), (lft, 1, 2, o), (lft, 2, 0, o), (lft, 2, 1, o), (lft, 2, 2, o)
                 , (bot, 0, 0, b), (bot, 0, 1, y), (bot, 0, 2, y), (bot, 1, 0, b), (bot, 1, 1, y), (bot, 1, 2, y), (bot, 2, 0, b), (bot, 2, 1, y), (bot, 2, 2, y) ]
      rotated = rotate leftL Cube.solvedCube
  in expected == rotated
  && testRotations leftL

testLeftR :: Bool
testLeftR = testRotations leftR

testTopL :: Bool
testTopL =
  let [top, frn, rgt, bck, lft, bot] = [Cube.Top ..]
      [w, g, r, b, o, y] = [Cube.White ..]
      expected = [ (top, 0, 0, w), (top, 0, 1, w), (top, 0, 2, w), (top, 1, 0, w), (top, 1, 1, w), (top, 1, 2, w), (top, 2, 0, w), (top, 2, 1, w), (top, 2, 2, w)
                 , (frn, 0, 0, o), (frn, 0, 1, o), (frn, 0, 2, o), (frn, 1, 0, g), (frn, 1, 1, g), (frn, 1, 2, g), (frn, 2, 0, g), (frn, 2, 1, g), (frn, 2, 2, g)
                 , (rgt, 0, 0, g), (rgt, 0, 1, g), (rgt, 0, 2, g), (rgt, 1, 0, r), (rgt, 1, 1, r), (rgt, 1, 2, r), (rgt, 2, 0, r), (rgt, 2, 1, r), (rgt, 2, 2, r)
                 , (bck, 0, 0, r), (bck, 0, 1, r), (bck, 0, 2, r), (bck, 1, 0, b), (bck, 1, 1, b), (bck, 1, 2, b), (bck, 2, 0, b), (bck, 2, 1, b), (bck, 2, 2, b)
                 , (lft, 0, 0, b), (lft, 0, 1, b), (lft, 0, 2, b), (lft, 1, 0, o), (lft, 1, 1, o), (lft, 1, 2, o), (lft, 2, 0, o), (lft, 2, 1, o), (lft, 2, 2, o)
                 , (bot, 0, 0, y), (bot, 0, 1, y), (bot, 0, 2, y), (bot, 1, 0, y), (bot, 1, 1, y), (bot, 1, 2, y), (bot, 2, 0, y), (bot, 2, 1, y), (bot, 2, 2, y) ]
      rotated = rotate topL Cube.solvedCube
  in expected == rotated
  && testRotations topL

testTopR :: Bool
testTopR = testRotations topR

testBottomL :: Bool
testBottomL =
  let [top, frn, rgt, bck, lft, bot] = [Cube.Top ..]
      [w, g, r, b, o, y] = [Cube.White ..]
      expected = [ (top, 0, 0, w), (top, 0, 1, w), (top, 0, 2, w), (top, 1, 0, w), (top, 1, 1, w), (top, 1, 2, w), (top, 2, 0, w), (top, 2, 1, w), (top, 2, 2, w)
                 , (frn, 0, 0, g), (frn, 0, 1, g), (frn, 0, 2, g), (frn, 1, 0, g), (frn, 1, 1, g), (frn, 1, 2, g), (frn, 2, 0, r), (frn, 2, 1, r), (frn, 2, 2, r)
                 , (rgt, 0, 0, r), (rgt, 0, 1, r), (rgt, 0, 2, r), (rgt, 1, 0, r), (rgt, 1, 1, r), (rgt, 1, 2, r), (rgt, 2, 0, b), (rgt, 2, 1, b), (rgt, 2, 2, b)
                 , (bck, 0, 0, b), (bck, 0, 1, b), (bck, 0, 2, b), (bck, 1, 0, b), (bck, 1, 1, b), (bck, 1, 2, b), (bck, 2, 0, o), (bck, 2, 1, o), (bck, 2, 2, o)
                 , (lft, 0, 0, o), (lft, 0, 1, o), (lft, 0, 2, o), (lft, 1, 0, o), (lft, 1, 1, o), (lft, 1, 2, o), (lft, 2, 0, g), (lft, 2, 1, g), (lft, 2, 2, g)
                 , (bot, 0, 0, y), (bot, 0, 1, y), (bot, 0, 2, y), (bot, 1, 0, y), (bot, 1, 1, y), (bot, 1, 2, y), (bot, 2, 0, y), (bot, 2, 1, y), (bot, 2, 2, y) ]
      rotated = rotate bottomL Cube.solvedCube
  in expected == rotated
  && testRotations bottomL

testBottomR :: Bool
testBottomR = testRotations bottomR

testFrontL :: Bool
testFrontL =
  let [top, frn, rgt, bck, lft, bot] = [Cube.Top ..]
      [w, g, r, b, o, y] = [Cube.White ..]
      expected = [ (top, 0, 0, w), (top, 0, 1, w), (top, 0, 2, w), (top, 1, 0, w), (top, 1, 1, w), (top, 1, 2, w), (top, 2, 0, r), (top, 2, 1, r), (top, 2, 2, r)
                 , (frn, 0, 0, g), (frn, 0, 1, g), (frn, 0, 2, g), (frn, 1, 0, g), (frn, 1, 1, g), (frn, 1, 2, g), (frn, 2, 0, g), (frn, 2, 1, g), (frn, 2, 2, g)
                 , (rgt, 0, 0, y), (rgt, 0, 1, r), (rgt, 0, 2, r), (rgt, 1, 0, y), (rgt, 1, 1, r), (rgt, 1, 2, r), (rgt, 2, 0, y), (rgt, 2, 1, r), (rgt, 2, 2, r)
                 , (bck, 0, 0, b), (bck, 0, 1, b), (bck, 0, 2, b), (bck, 1, 0, b), (bck, 1, 1, b), (bck, 1, 2, b), (bck, 2, 0, b), (bck, 2, 1, b), (bck, 2, 2, b)
                 , (lft, 0, 0, o), (lft, 0, 1, o), (lft, 0, 2, w), (lft, 1, 0, o), (lft, 1, 1, o), (lft, 1, 2, w), (lft, 2, 0, o), (lft, 2, 1, o), (lft, 2, 2, w)
                 , (bot, 0, 0, o), (bot, 0, 1, o), (bot, 0, 2, o), (bot, 1, 0, y), (bot, 1, 1, y), (bot, 1, 2, y), (bot, 2, 0, y), (bot, 2, 1, y), (bot, 2, 2, y) ]
      rotated = rotate frontL Cube.solvedCube
  in expected == rotated
  && testRotations frontL

testFrontR :: Bool
testFrontR = testRotations frontR

testBackL :: Bool
testBackL =
  let [top, frn, rgt, bck, lft, bot] = [Cube.Top ..]
      [w, g, r, b, o, y] = [Cube.White ..]
      expected = [ (top, 0, 0, o), (top, 0, 1, o), (top, 0, 2, o), (top, 1, 0, w), (top, 1, 1, w), (top, 1, 2, w), (top, 2, 0, w), (top, 2, 1, w), (top, 2, 2, w)
                 , (frn, 0, 0, g), (frn, 0, 1, g), (frn, 0, 2, g), (frn, 1, 0, g), (frn, 1, 1, g), (frn, 1, 2, g), (frn, 2, 0, g), (frn, 2, 1, g), (frn, 2, 2, g)
                 , (rgt, 0, 0, r), (rgt, 0, 1, r), (rgt, 0, 2, w), (rgt, 1, 0, r), (rgt, 1, 1, r), (rgt, 1, 2, w), (rgt, 2, 0, r), (rgt, 2, 1, r), (rgt, 2, 2, w)
                 , (bck, 0, 0, b), (bck, 0, 1, b), (bck, 0, 2, b), (bck, 1, 0, b), (bck, 1, 1, b), (bck, 1, 2, b), (bck, 2, 0, b), (bck, 2, 1, b), (bck, 2, 2, b)
                 , (lft, 0, 0, y), (lft, 0, 1, o), (lft, 0, 2, o), (lft, 1, 0, y), (lft, 1, 1, o), (lft, 1, 2, o), (lft, 2, 0, y), (lft, 2, 1, o), (lft, 2, 2, o)
                 , (bot, 0, 0, y), (bot, 0, 1, y), (bot, 0, 2, y), (bot, 1, 0, y), (bot, 1, 1, y), (bot, 1, 2, y), (bot, 2, 0, r), (bot, 2, 1, r), (bot, 2, 2, r) ]
      rotated = rotate backL Cube.solvedCube
  in expected == rotated
  && testRotations backL

testBackR :: Bool
testBackR = testRotations backR

testCombined :: Bool
testCombined =
  let [top, frn, rgt, bck, lft, bot] = [Cube.Top ..]
      [w, g, r, b, o, y] = [Cube.White ..]
      expected = [ (top, 0, 0, o), (top, 0, 1, o), (top, 0, 2, b), (top, 1, 0, o), (top, 1, 1, w), (top, 1, 2, b), (top, 2, 0, o), (top, 2, 1, r), (top, 2, 2, r)
                 , (frn, 0, 0, b), (frn, 0, 1, g), (frn, 0, 2, w), (frn, 1, 0, y), (frn, 1, 1, g), (frn, 1, 2, w), (frn, 2, 0, y), (frn, 2, 1, y), (frn, 2, 2, b)
                 , (rgt, 0, 0, g), (rgt, 0, 1, r), (rgt, 0, 2, w), (rgt, 1, 0, g), (rgt, 1, 1, r), (rgt, 1, 2, w), (rgt, 2, 0, y), (rgt, 2, 1, y), (rgt, 2, 2, o)
                 , (bck, 0, 0, r), (bck, 0, 1, b), (bck, 0, 2, g), (bck, 1, 0, r), (bck, 1, 1, b), (bck, 1, 2, w), (bck, 2, 0, y), (bck, 2, 1, y), (bck, 2, 2, g)
                 , (lft, 0, 0, w), (lft, 0, 1, w), (lft, 0, 2, w), (lft, 1, 0, b), (lft, 1, 1, o), (lft, 1, 2, o), (lft, 2, 0, y), (lft, 2, 1, g), (lft, 2, 2, r)
                 , (bot, 0, 0, g), (bot, 0, 1, g), (bot, 0, 2, r), (bot, 1, 0, o), (bot, 1, 1, y), (bot, 1, 2, r), (bot, 2, 0, o), (bot, 2, 1, b), (bot, 2, 2, b) ]
      rotation = bottomL <> leftL <> backL <> rightL <> frontL <> topL
      rotated = rotate rotation Cube.solvedCube
  in expected == rotated
  && testRotations backL

testNameMatrixRotation :: Bool
testNameMatrixRotation = and $ do
  (r, n) <- rotations
  let n' = nameMatrixRotation r
  return $ n == n'

testInverse :: Bool
testInverse = and $ do
  (r, _) <- rotations
  let (Just r') = inverse r
  let (Just r'') = inverse r'
  return $ r == r''

tests = [ ("Rotation.topToFront", testTopToFront)
        , ("Rotation.topToBack", testTopToBack)
        , ("Rotation.topToRight", testTopToRight)
        , ("Rotation.topToLeft", testTopToLeft)
        , ("Rotation.topTwistRight", testTopTwistRight)
        , ("Rotation.topTwistLeft", testTopTwistLeft)
        , ("Rotation.rightL", testRightL)
        , ("Rotation.rightR", testRightR)
        , ("Rotation.leftL", testLeftL)
        , ("Rotation.leftR", testLeftR)
        , ("Rotation.topL", testTopL)
        , ("Rotation.topR", testTopR)
        , ("Rotation.bottomL", testBottomL)
        , ("Rotation.bottomR", testBottomR)
        , ("Rotation.frontL", testFrontL)
        , ("Rotation.frontR", testFrontR)
        , ("Rotation.backL", testBackL)
        , ("Rotation.backR", testBackR)
        , ("combined", testCombined)
        , ("Rotation.nameMatrixRotation", testNameMatrixRotation)
        , ("Rotation.inverse", testInverse)
        ]
