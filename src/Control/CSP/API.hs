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
, VarDomains(..)
) where

import Control.CSP.Internal.Types
import Control.CSP.Internal.VarDomains
import Control.CSP.Algorithms.AC3

data CSP v d where
  CSP :: (Ord v)
      => BinaryConstraintSet v d
      -> UnaryConstraintSet v d
      -> VarDomains v d
      -> CSP v d

runAC3 :: (Bounded v, Enum v, Ord v, Eq v, Eq d) => CSP v d -> Maybe (CSP v d)
runAC3 (CSP bc us vd) = CSP bc us <$> updateBinaryConstraints bc vd
