module Control.CSP.Internal.Satisfiable where

import Control.CSP.Internal.Assignment
import Control.CSP.Internal.Types
import Data.List

class Satisfiable a where
  isSatisfied :: (Ord v, Eq d) => a v d -> Assignment v d -> Bool

instance Satisfiable BinaryConstraint where
  isSatisfied (BC (v1, v2, ds)) a = any (`assignCmp` (get v1 a, get v2 a)) ds

instance Satisfiable UnaryConstraint where
  isSatisfied (UC (v1, ds)) a = elem (get v1 a) $ map return ds

isSatisifiableWith :: (Ord v, Eq d)
                   => BinaryConstraint v d
                   -> [d]
                   -> Assignment v d
                   -> Bool
isSatisifiableWith bc@(BC (v1, v2, ds)) ds2 a =
  any (isSatisfied bc . flip (assign v2) a) ds2

isComplete :: (Ord v, Eq d) => BinaryConstraintSet v d -> Assignment v d -> Bool
isComplete d a = all (`isSatisfied` a) d
