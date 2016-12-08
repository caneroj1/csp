{-# LANGUAGE GADTs #-}

module Control.CSP.CSP
(
  CSP(..)
) where

import Control.CSP.Internal.Types
import Control.CSP.Internal.VarDomains

data CSP v d where
  CSP :: (Ord v)
      => BinaryConstraintSet v d
      -> UnaryConstraintSet v d
      -> VarDomains v d
      -> CSP v d
