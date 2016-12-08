module Control.CSP.API
(
  makeDomains
, mkBinary
, mkUnary
, solve
, BinaryConstraint
, UnaryConstraint
, BinaryConstraintSet
, UnaryConstraintSet

, runAC3
, runNC
, VarDomains(..)
, Assignment(..)
) where

import Control.CSP.Internal.Assignment
import Control.CSP.Internal.Types
import Control.CSP.Internal.VarDomains
import Control.CSP.Algorithms.AC3
import Control.CSP.Algorithms.NodeConsistency
import Control.CSP.Search.Backtrack
import Control.CSP.CSP

runAC3 :: (Bounded v, Enum v, Ord v, Eq v, Eq d) => CSP v d -> Maybe (CSP v d)
runAC3 (CSP bc uc vd) = CSP bc uc <$> updateBinaryConstraints bc vd

runNC :: (Bounded v, Enum v, Ord v, Eq v, Eq d) => CSP v d -> Maybe (CSP v d)
runNC (CSP bc uc vd) = CSP bc uc <$> removeUnaryConstraints uc vd

solve :: (Bounded v, Enum v, Ord v, Eq v, Eq d, Show v, Show d)
      => CSP v d
      -> Maybe (Assignment v d)
solve csp = runNC csp >>= runAC3 >>= (\(CSP bc _ d) -> backtrack bc d)
