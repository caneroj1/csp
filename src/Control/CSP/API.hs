{-# LANGUAGE GADTs #-}

module Control.CSP.API
(
  makeDomains
, mkBinary
, mkUnary
, CSP(..)
, BinaryConstraint
, UnaryConstraint
, BinaryConstraintSet
, UnaryConstraintSet

, runAC3
, runNC
, VarDomains(..)
) where

import Control.CSP.Internal.Types
import Control.CSP.Internal.VarDomains
import Control.CSP.Algorithms.AC3
import Control.CSP.Algorithms.NodeConsistency

data CSP v d where
  CSP :: (Ord v)
      => BinaryConstraintSet v d
      -> UnaryConstraintSet v d
      -> VarDomains v d
      -> CSP v d

runAC3 :: (Bounded v, Enum v, Ord v, Eq v, Eq d) => CSP v d -> Maybe (CSP v d)
runAC3 (CSP bc uc vd) = CSP bc uc <$> updateBinaryConstraints bc vd

runNC :: (Bounded v, Enum v, Ord v, Eq v, Eq d) => CSP v d -> Maybe (CSP v d)
runNC (CSP bc uc vd) = CSP bc uc <$> removeUnaryConstraints uc vd
