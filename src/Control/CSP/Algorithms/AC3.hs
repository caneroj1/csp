{-# LANGUAGE TemplateHaskell #-}

module Control.CSP.Algorithms.AC3
(
  updateBinaryConstraints
) where

import Control.CSP.Internal.Assignment hiding (get)
import Control.CSP.Internal.Satisfiable
import Control.CSP.Internal.Types
import Control.CSP.Internal.Utils
import Control.CSP.Internal.VarDomains
import Control.Monad.State

import Control.Lens hiding (assign)

data AC3State v d = AC3 {
    _adjs :: Adj v d
  , _doms :: VarDomains v d
  , _assn :: Assignment v d
  }
makeLenses ''AC3State

updateBinaryConstraints :: (Bounded v, Enum v, Ord v, Eq d)
                        => BinaryConstraintSet v d
                        -> VarDomains v d
                        -> Maybe (VarDomains v d)
updateBinaryConstraints bs d =
  let
    ns = adj bs
    (success, d') = (runState (ac3 bs) $ AC3 ns d initial)
    in if success then return $ _doms d' else Nothing
  where
    ac3 :: (Enum v, Bounded v, Ord v, Eq d)
        => BinaryConstraintSet v d
        -> State (AC3State v d) Bool
    ac3 []     = return True
    ac3 (b:bs) = do
      s <- get
      let (d1, d2) = domainOfEachArc b (_doms s)
          arc      = getArc b
          old      = _assn s
          mkAssn d = assign (fst arc) d old
          d1'      = [d | d <- d1, isSatisifiableWith b d2 (mkAssn d)]
      if null d1'
        then return False
        else modify' (\s -> s & doms %~ updateDomain (fst arc) d1')
             >> ac3 (newArcList bs (s ^. adjs) (fst arc))
    newArcList bs as v = maybe bs (bs ++) $ lookup v as
