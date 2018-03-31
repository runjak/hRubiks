module Strategy.TestOrientation where

import Prelude hiding (Right, Left)
import Numeric.LinearAlgebra ((<>))
import qualified Control.Monad.Writer as Writer
import qualified Data.DList as DList

import Cube (Side(..), Color(..))
import qualified Cube as Cube
import qualified Rotation as Rotation
import qualified RotationPath as RotationPath
import qualified Strategy.Orientation as Orientation

testSides :: Bool
testSides =
  let expected = [
          (Top, White)
        , (Front, Green)
        , (Right, Red)
        , (Back, Blue)
        , (Left, Orange)
        , (Bottom, Yellow)
        ]
      actual = Orientation.sides Cube.solvedCube
  in expected == actual

testIsColorOnSide :: Bool
testIsColorOnSide =
  let t = Orientation.isColorOnSide White Top Cube.solvedCube
      f = Orientation.isColorOnSide Blue Bottom Cube.solvedCube
  in t == True && f == False

testForColorOnTop :: Bool
testForColorOnTop = and $ do
  color <- [Cube.White ..]
  let (prefix, suffix) = Orientation.forColorOnTop color Cube.solvedCube
      cube = Rotation.rotate prefix Cube.solvedCube
      isOnTop = Orientation.isColorOnSide color Cube.Top cube
      isIdentity = (suffix <> prefix) == Rotation.identity
  return $ isIdentity && isOnTop

testPutColorOnTop :: Bool
testPutColorOnTop =
  let expectedSides = [
          (Top, Yellow)
        , (Front, Blue)
        , (Right, Red)
        , (Back, Green)
        , (Left, Orange)
        , (Bottom, White)
        ]
      expectedRotations = [Rotation.topToFront <> Rotation.topToFront]
      (cube, rotationLog) = Writer.runWriter $
        Orientation.putColorOnTop Yellow Cube.solvedCube
      actualSides = Orientation.sides cube
  in actualSides == expectedSides && expectedRotations == DList.toList rotationLog

testWithColorOnTop :: Bool
testWithColorOnTop =
  let (cube, rotationLog) = Writer.runWriter $
        Orientation.withColorOnTop Green (RotationPath.rotate Rotation.topL) Cube.solvedCube
      [top, frn, rgt, bck, lft, bot] = [Cube.Top ..]
      [w, g, r, b, o, y] = [Cube.White ..]
      expectedCube = [
          (top,0,0,w),(top,0,1,w),(top,0,2,w),(top,1,0,w),(top,1,1,w),(top,1,2,w),(top,2,0,r),(top,2,1,r),(top,2,2,r)
        , (frn,0,0,g),(frn,0,1,g),(frn,0,2,g),(frn,1,0,g),(frn,1,1,g),(frn,1,2,g),(frn,2,0,g),(frn,2,1,g),(frn,2,2,g)
        , (rgt,0,0,y),(rgt,0,1,r),(rgt,0,2,r),(rgt,1,0,y),(rgt,1,1,r),(rgt,1,2,r),(rgt,2,0,y),(rgt,2,1,r),(rgt,2,2,r)
        , (bck,0,0,b),(bck,0,1,b),(bck,0,2,b),(bck,1,0,b),(bck,1,1,b),(bck,1,2,b),(bck,2,0,b),(bck,2,1,b),(bck,2,2,b)
        , (lft,0,0,o),(lft,0,1,o),(lft,0,2,w),(lft,1,0,o),(lft,1,1,o),(lft,1,2,w),(lft,2,0,o),(lft,2,1,o),(lft,2,2,w)
        , (bot,0,0,o),(bot,0,1,o),(bot,0,2,o),(bot,1,0,y),(bot,1,1,y),(bot,1,2,y),(bot,2,0,y),(bot,2,1,y),(bot,2,2,y)]
      expectedRotations = [Rotation.frontL]
  in cube == expectedCube && DList.toList rotationLog == expectedRotations

tests :: [(String, Bool)]
tests = [
    ("Strategy.Orientation.sides", testSides)
  , ("Strategy.Orientation.isColorOnSide", testIsColorOnSide)
  , ("Strategy.Orientation.forColorOnTop", testForColorOnTop)
  , ("Strategy.Orientation.putColorOnTop", testPutColorOnTop)
  , ("Strategy.Orientation.withColorOnTop", testWithColorOnTop)
  ]
