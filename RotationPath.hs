module RotationPath where

import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as Writer
import Data.DList (DList)
import qualified Data.DList as DList
import Numeric.LinearAlgebra ((<>))

import Cube (Cube)
import qualified Cube as Cube
import Rotation (MatrixRotation)
import qualified Rotation as Rotation

type RotationPath a = Writer (DList MatrixRotation) a
type CubeMutation = Cube -> RotationPath Cube

rotate :: MatrixRotation -> CubeMutation
rotate rotation cube = do
  let cube' = Rotation.toCubeRotation (Rotation.toVectorRotation rotation) cube
  Writer.tell $ DList.singleton rotation
  return cube'

between :: MatrixRotation -> MatrixRotation -> CubeMutation -> CubeMutation
between prefix suffix λ cube = do
  let (cube', rotationLog) = Writer.runWriter . λ $ Rotation.rotate prefix cube
  Writer.tell $ DList.map (\r -> suffix <> r <> prefix) rotationLog
  return $ Rotation.rotate suffix cube'
