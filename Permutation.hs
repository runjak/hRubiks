module Permutation where

import Rotation (MatrixRotation, identity)
import qualified Rotation as Rotation

import qualified Numeric.LinearAlgebra as LA

type N = Integer

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
multiplierMappings = map mappingFor [0..]
  where
    mappingFor :: Integer -> Integer -> Integer
    mappingFor n m
      | n == m = 0
      | n < m = m
      | otherwise = m + 1

fromRotation :: MatrixRotation -> N
fromRotation = sum . zipWith3 go facs multiplierMappings . toNumbers
  where
    go fac multiplierMapping n = fac * multiplierMapping n

-- toRotation :: N -> MatrixRotation
toRotation = go facs
  where
    go [] _ = []
    go (f:fs) n
      | n >= f =
        let (d, r) = n `divMod` f
        in d : go fs r
      | otherwise = 0 : go fs n
