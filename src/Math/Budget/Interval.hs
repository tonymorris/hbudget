module Math.Budget.Interval
(
  Interval
, fixedIntervalL
, arbitraryIntervalL
, isOnceOffInterval
, fixedInterval
, arbitraryInterval
, onceOffInterval
) where

import Math.Budget.FixedPeriod
import Data.Lens.Partial.Common
import Control.Comonad.Trans.Store

data Interval =
  Fixed FixedPeriod
  | Arbitrary FixedPeriods
  | OnceOff
  deriving (Eq, Ord)

fixedIntervalL ::
  PartialLens Interval FixedPeriod
fixedIntervalL =
  PLens $ \p -> case p of
    Fixed n -> Just (store fixedInterval n)
    _ -> Nothing

arbitraryIntervalL ::
  PartialLens Interval FixedPeriods
arbitraryIntervalL =
  PLens $ \p -> case p of
    Arbitrary n -> Just (store arbitraryInterval n)
    _ -> Nothing

isOnceOffInterval ::
  Interval
  -> Bool
isOnceOffInterval OnceOff =
  True
isOnceOffInterval _ =
  False

fixedInterval ::
  FixedPeriod
  -> Interval
fixedInterval =
  Fixed

arbitraryInterval ::
  FixedPeriods
  -> Interval
arbitraryInterval =
  Arbitrary

onceOffInterval ::
    Interval
onceOffInterval =
  OnceOff
