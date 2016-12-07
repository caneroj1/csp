module Control.CSP.Search.Backtrack
(
  backtrack
) where

import Control.CSP.API
import Control.CSP.Internal.Assignment
import Control.CSP.Internal.Satisfiable
import Control.CSP.Internal.Types
import Control.CSP.Internal.VarDomains

backtrack :: (Bounded v, Enum v, Ord v, Eq d)
          => BinaryConstraintSet v d
          -> VarDomains v d
          -> Maybe (Assignment v d)
backtrack = doBacktrack initial
  where
    doBacktrack assn bcs domain
      | isComplete bcs assn = return assn
      | otherwise           = Nothing
