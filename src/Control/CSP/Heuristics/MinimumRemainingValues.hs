module Control.CSP.Heuristics.MinimumRemainingValues
(
  minimumRemainingValues
)
where

import Control.CSP.Internal.Assignment
import Control.CSP.Internal.Types
import Control.CSP.Internal.Utils
import Control.CSP.Internal.VarDomains
import Data.Bifunctor
import qualified Data.IntMap as I
import Data.List
import Data.Maybe
import Data.Ord

minimumRemainingValues :: (Ord v, Enum v, Eq d, Bounded v)
                       => Assignment v d
                       -> VarDomains v d
                       -> BinaryConstraintSet v d
                       -> (VarDomains v d, v)
minimumRemainingValues a d = applyKeep (chooseFrom a) . foldl' (constrain a) d

constrain :: (Ord v, Enum v, Eq d)
          => Assignment v d
          -> VarDomains v d
          -> BinaryConstraint v d
          -> VarDomains v d
constrain as d (BC (v1, v2, ds))
  | v1Assigned && v2Assigned = d
  | v1Assigned               = domain' v2 $! getFromBC ((== d1) . fst) snd
  | v2Assigned               = domain' v1 $! getFromBC ((== d2) . snd) fst
  | otherwise                = d
  where
    v1Choice = get v1 as
    v2Choice = get v2 as
    v1Assigned = isJust v1Choice
    v2Assigned = isJust v2Choice
    Just d1 = v1Choice
    Just d2 = v2Choice
    domain' v vs = flip (updateDomain v) d $! vs `intersect` domainOf d v
    getFromBC f c = map c $ filter f ds

chooseFrom :: (Enum v, Ord v) => Assignment v d -> VarDomains v d -> v
chooseFrom a = toEnum                      .
               fst                         .
               minimumBy (comparing snd)   .
               map       (bimap id length) .
               filter    (unassigned a)    .
               I.toList                    .
               getDomains
 where unassigned a = isNotAssigned a . toEnum . fst
