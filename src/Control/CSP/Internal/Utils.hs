module Control.CSP.Internal.Utils where

import Control.CSP.Internal.Types

allEnums :: (Enum a, Bounded a) => [a]
allEnums = minBound `enumFromTo` maxBound

getArc :: BinaryConstraint v d -> (v, v)
getArc (BC (v1, v2, _)) = (v1, v2)

type Adj v d = [(v, BinaryConstraintSet v d)]

adj :: (Eq v, Enum v, Bounded v) => BinaryConstraintSet v d -> Adj v d
adj bs = do
  v <- allEnums
  return (v, filter (isNeighborOf v) bs)
  where
    isNeighborOf v (BC (_, v', _)) = v == v'

binary :: Bool -> Int
binary True = 1
binary _    = 0

mapKeep :: (a -> b) -> [a] -> [(a, b)]
mapKeep f = map (\a -> (a, f a))

applyKeep :: (a -> b) -> a -> (a, b)
applyKeep f a = (a, f a)
