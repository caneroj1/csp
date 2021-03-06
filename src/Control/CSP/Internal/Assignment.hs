module Control.CSP.Internal.Assignment
(
  initial
, get
, getAllUnassigned
, assign
, unassign
, isAssigned
, isNotAssigned
, isNull
, getAssign
, Assignment
)
where

import Control.CSP.Internal.Types
import Control.CSP.Internal.Utils
import Data.Bifunctor
import qualified Data.Map as M
import Data.Maybe

initial :: (Ord v, Bounded v, Enum v) => Assignment v d
initial = Assignment . M.fromList $ zip allEnums (repeat Nothing)

get :: (Ord v) => v -> Assignment v d -> Maybe d
get v = fromJust . M.lookup v . getAssign

assign :: (Ord v) => v -> d -> Assignment v d -> Assignment v d
assign v d = reassign v (Just d)

unassign :: (Ord v) => v -> Assignment v d -> Assignment v d
unassign v = reassign v Nothing

reassign :: (Ord v) => v -> Maybe d -> Assignment v d -> Assignment v d
reassign v d = Assignment . M.update (const (return d)) v . getAssign

isAssigned :: (Ord v) => v -> Assignment v d -> Bool
isAssigned v = isJust . fromJust . M.lookup v . getAssign

isNotAssigned :: (Ord v) => Assignment v d -> v -> Bool
isNotAssigned a = not . flip isAssigned a

isNull :: (Ord v) => Assignment v d -> Bool
isNull = all isNothing . M.elems . getAssign

getAllUnassigned :: (Ord v, Bounded v, Enum v) => Assignment v d -> [v]
getAllUnassigned a = filter (not . flip isAssigned a) allEnums

newtype Assignment v d = Assignment {
  getAssign :: M.Map v (Maybe d)
}
