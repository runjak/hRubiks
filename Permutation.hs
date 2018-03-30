module Permutation where

import Rotation (MatrixRotation, identity)
import qualified Rotation as Rotation

import qualified Numeric.LinearAlgebra as LA

type N = Integer

fromRotation :: MatrixRotation -> N
fromRotation = sum . zipWith3 go facs multiplierMappings . toNumbers
  where
    go fac multiplierMapping n = fac * multiplierMapping n

toNumbers :: MatrixRotation -> [N]
toNumbers = map (go 0 . LA.toList) . LA.toColumns
  where
    go n [] = n
    go n (f:fs)
      | f /= 0 = n
      | otherwise = go (n + 1) fs

facs :: [N]
facs =
  let n = 9 * 6
  in map fac [n - 1, n - 2 .. 0]
  where
    fac 0 = 1
    fac n = n * fac (n - 1)

multiplierMappings :: [N -> N]
multiplierMappings = map multiplierFor [0..]
  where
    multiplierFor :: Integer -> Integer -> Integer
    multiplierFor n m
      | n == m = 0
      | n < m = m
      | otherwise = m + 1
