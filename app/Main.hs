module Main where

import Control.CSP.API
import Control.CSP.CSP
import Control.CSP.Internal.Assignment
import Control.CSP.Heuristics.LeastConstraining
import Control.CSP.Heuristics.Degree

data Aus = WA | NT | Q | NSW | V | SA | T
  deriving (Ord, Eq, Show, Bounded, Enum)

data Color = R | G | B
  deriving (Eq, Show)

domains :: Aus -> [Color]
domains _ = [R, G, B]

allDiff :: [(Color, Color)]
allDiff = [(R, G), (R, B), (G, R), (G, B), (B, R), (B, G)]

bcs :: BinaryConstraintSet Aus Color
bcs = [
    mkBinary SA WA allDiff
  , mkBinary SA NT allDiff
  , mkBinary SA Q allDiff
  , mkBinary SA NSW allDiff
  , mkBinary SA V allDiff
  , mkBinary WA NT allDiff
  , mkBinary NT Q allDiff
  , mkBinary Q NSW allDiff
  , mkBinary NSW V allDiff
  ]

ucs :: UnaryConstraintSet Aus Color
ucs = []

problem :: CSP Aus Color
problem = CSP bcs ucs (makeDomains domains)

handleFilter :: Maybe (CSP Aus Color) -> IO ()
handleFilter Nothing                         = putStrLn "Got nothing"
handleFilter (Just (CSP _ _ (VarDomains v))) = print v

handleAssignment :: Maybe (Assignment Aus Color) -> IO ()
handleAssignment Nothing               = putStrLn "Got nothing"
handleAssignment (Just (Assignment m)) = print m

main :: IO ()
main = handleAssignment $ solve problem
