module Control.CSP.Search.Backtrack
(
  backtrack
) where

import Control.CSP.CSP
import Control.CSP.Heuristics.Degree
import Control.CSP.Heuristics.LeastConstraining
import Control.CSP.Heuristics.MinimumRemainingValues
import Control.CSP.Internal.Assignment
import Control.CSP.Internal.Satisfiable
import Control.CSP.Internal.Types
import Control.CSP.Internal.VarDomains
import Control.Monad

backtrack :: (Bounded v, Enum v, Ord v, Eq d)
          => BinaryConstraintSet v d
          -> VarDomains v d
          -> Maybe (Assignment v d)
backtrack = doBacktrack initial
  where
    doBacktrack assn bcs domain
      | isComplete bcs assn = return assn
      | otherwise           =
        msum (mzero : do
          let (domain', v) = getNextChoice bcs domain assn
          d <- leastConstraining v domain assn bcs
          let assn'    = assign v d assn
              domain'' = use v d domain'
          return $ doBacktrack assn' bcs domain'')

getNextChoice :: (Bounded v, Enum v, Ord v, Eq d)
              => BinaryConstraintSet v d
              -> VarDomains v d
              -> Assignment v d
              -> (VarDomains v d, v)
getNextChoice bcs d a
  | isNull a  = (d, degree a d bcs)
  | otherwise = minimumRemainingValues a d bcs
