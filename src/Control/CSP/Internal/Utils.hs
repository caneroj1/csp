module Control.CSP.Internal.Utils where

import Control.CSP.Internal.Types
import Data.Bifunctor

allEnums :: (Enum a, Bounded a) => [a]
allEnums = minBound `enumFromTo` maxBound

getArc :: BinaryConstraint v d -> (v, v)
getArc (BC (v1, v2, _)) = (v1, v2)

mapKeep :: (a -> b) -> [a] -> [(a, b)]
mapKeep f = map (\a -> (a, f a))

applyKeep :: (a -> b) -> a -> (a, b)
applyKeep f a = (a, f a)

mcmp :: (Monad m, Eq a, Eq (m a)) => (a, a) -> (m a, m a) -> Bool
mcmp f s = s == bimap return return f
