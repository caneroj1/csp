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

import Debug.Trace

-- now that we have a variable, we need to choose
-- from its domain.
-- for each value in the domain, we need to check:
--  how many values it eliminates from the domains of
--  other unassigned variables
--    so given a variable and an element from its domain,
--    how do we check how many other elements it removes?
--      given a list of the other unassigned variables,
--      do the following for each:
--        get all of the elements in its domain, sum the
--        number of constraints that will be violated if
--        you choose that arrangement

leastConstraining :: (Ord v, Enum v, Eq d, Bounded v, Show v)
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
        in [sum $ map (binary . flip notSatisfied a') bcs]
