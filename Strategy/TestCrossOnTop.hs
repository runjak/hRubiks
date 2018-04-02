module Strategy.TestCrossOnTop where

import Prelude hiding (Left, Right)

import Cube (Side(..))
import qualified Cube as Cube
import qualified Strategy.CrossOnTop as CrossOnTop

testEdges :: Bool
testEdges = all (\((s1, _, _), (s2, _, _)) -> Cube.adjacentSides s1 s2) CrossOnTop.edges

testProtectedEdges =
  let tops = take 4 CrossOnTop.edges
      withoutTwoTops = CrossOnTop.protectedEdges (head tops) (tops !! 1)
      withoutTwoOk = withoutTwoTops == drop 2 tops
      withoutOneTop = CrossOnTop.protectedEdges (head tops) $ last CrossOnTop.edges
      withoutOneOk = withoutOneTop == tail tops
      [e1, e2] = take 2 $ drop 4 CrossOnTop.edges
      withoutBothTop = CrossOnTop.protectedEdges e1 e2
      withoutBothOk = withoutBothTop == tops
  in withoutTwoOk && withoutOneOk && withoutBothOk

tests :: [(String, Bool)]
tests = [
    ("Strategy.CrossOnTop.edges", testEdges)
  , ("Strategy.CrossOnTop.protectedEdges", testProtectedEdges)
  ]
