{-# LANGUAGE TemplateHaskell #-}

module Control.CSP.Algorithms.NodeConsistency
(
  removeUnaryConstraints
) where

import Control.CSP.Internal.Assignment hiding (get)
import Control.CSP.Internal.Satisfiable
import Control.CSP.Internal.Types
import Control.CSP.Internal.Utils
import Control.CSP.Internal.VarDomains
import Control.Monad.State

import Control.Lens hiding (assign)

data NCState v d = NC {
    _doms :: VarDomains v d
  }
makeLenses ''NCState

removeUnaryConstraints :: (Bounded v, Enum v, Ord v, Eq d)
                        => UnaryConstraintSet v d
                        -> VarDomains v d
                        -> Maybe (VarDomains v d)
removeUnaryConstraints us d =
  let
    (success, d') = (runState (nodeConsistency us) $ NC d)
    in if success then return $ _doms d' else Nothing
  where
    nodeConsistency :: (Enum v, Bounded v, Ord v, Eq d)
                    => UnaryConstraintSet v d
                    -> State (NCState v d) Bool
    nodeConsistency []              = return True
    nodeConsistency (UC (u, ds):us) = do
      s <- get
      let d' = filter (`elem` ds) $ domainOf (_doms s) u
      if null d'
        then return False
        else modify' (\s -> s & doms %~ updateDomain u d')
             >> nodeConsistency us
