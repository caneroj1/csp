module Control.CSP.Internal.Types where

import qualified Data.IntMap as I
import qualified Data.Map as M

newtype UnaryConstraint v d =
  UC {
    unUnary :: (v, [d])
  }

instance (Show v, Show d) => Show (UnaryConstraint v d) where
  show (UC (v, ds)) = show v ++ " in " ++ show ds

newtype BinaryConstraint v d =
  BC {
    unBinary :: (v, v, [(d, d)])
  }

mkBinary :: v -> v -> [(d, d)] -> BinaryConstraint v d
mkBinary v1 v2 ds = BC (v1, v2, ds)

mkUnary :: v -> [d] -> UnaryConstraint v d
mkUnary v ds = UC (v, ds)

instance (Show v, Show d) => Show (BinaryConstraint v d) where
  show (BC (v1, v2, ds)) = show (v1, v2) ++ " in " ++ show ds

type BinaryConstraintSet v d = [BinaryConstraint v d]
type UnaryConstraintSet v d = [UnaryConstraint v d]
