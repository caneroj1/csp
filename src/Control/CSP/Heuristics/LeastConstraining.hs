module Control.CSP.Heuristics.LeastConstraining
(
  leastConstraining
)
where

import Control.CSP.Internal.Assignment
import Control.CSP.Internal.Satisfiable
import Control.CSP.Internal.Types
import Control.CSP.Internal.Utils
import Control.CSP.Internal.VarDomains
import Data.Foldable
import Data.List
import Data.Ord

leastConstraining :: (Ord v, Enum v, Eq d, Bounded v)
                  => v
                  -> VarDomains v d
                  -> Assignment v d
                  -> BinaryConstraintSet v d
                  -> [d]
leastConstraining chosen d a bcs =
  map fst . sortBy (comparing snd) . mapKeep numEliminated $ domainOf d chosen
  where
    remaining       = filter (/= chosen) $ getAllUnassigned a
    numEliminated d1 = sum $ do
      unassn <- remaining
      minimum [numUnsatisfied d1 unassn d2 | d2 <- domainOf d unassn]
    numUnsatisfied d1 v2 d2 =
      let a' = assign v2 d2 $ assign chosen d1 a
        in [sum $ map (fromEnum . flip notSatisfied a') bcs]
