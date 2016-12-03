module Control.CSP.Internal.VarDomains
(
  makeDomains
, domainOf
, updateDomain
, domainOfEachArc
, VarDomains(..)
)
where

import Control.CSP.Internal.Types
import Control.CSP.Internal.Utils
import Data.Bifunctor
import qualified Data.IntMap as I
import Data.List
import Data.Maybe

newtype VarDomains v d = VarDomains {
    getDomains :: I.IntMap [d]
  }

makeDomains :: (Enum v, Bounded v) => (v -> [d]) -> VarDomains v d
makeDomains f = VarDomains $ foldl' nm I.empty allEnums
  where nm m e = I.insert (fromEnum e) (f e) m

domainOf :: (Enum v) => VarDomains v d -> v -> [d]
domainOf d v = fromJust . I.lookup (fromEnum v) $ getDomains d

updateDomain :: (Enum v) => v -> [d] -> VarDomains v d -> VarDomains v d
updateDomain v ds d = VarDomains $ I.update (Just . const ds) iv $ getDomains d
  where iv = fromEnum v

domainOfEachArc :: (Enum v)
                => BinaryConstraint v d
                -> VarDomains v d
                -> ([d], [d])
domainOfEachArc (BC (v1, v2, _)) d = bimap (domainOf d) (domainOf d) (v1, v2)
