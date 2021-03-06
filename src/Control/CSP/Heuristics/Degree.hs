module Control.CSP.Heuristics.Degree
(
  degree
)
where

import Control.CSP.Internal.Assignment
import Control.CSP.Internal.Types
import Control.CSP.Internal.Utils
import Control.CSP.Internal.VarDomains
import Data.Foldable
import Data.Ord

degree :: (Ord v, Enum v, Eq d, Bounded v)
        => Assignment v d
        -> VarDomains v d
        -> BinaryConstraintSet v d
        -> v
degree a _ bs =
  fst . minimumBy (comparing snd) $ mapKeep (toConstraintIndex unassn bs) unassn
  where unassn = getAllUnassigned a

type Unassigned v = [v]

toConstraintIndex :: (Ord v, Enum v, Eq d)
                  => Unassigned v
                  -> BinaryConstraintSet v d
                  -> v
                  -> Int
toConstraintIndex us bs v = sum $ map (isInConstraint v us) bs
  where
    isInConstraint v us (BC (v1, v2, _))
      | v1 == v   = fromEnum $ v2 `elem` us
      | v2 == v   = fromEnum $ v1 `elem` us
      | otherwise = 0
