{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Budget.Interval
(
  Interval
, isOnceOffInterval
, fixedInterval
, arbitraryInterval
, onceOffInterval
) where

import Math.Budget.Lens.ArbitraryIntervalL
import Math.Budget.Lens.FixedIntervalL
import Math.Budget.FixedPeriod
import Data.Lens.Partial.Common
import Control.Comonad.Trans.Store

data Interval =
  Fixed FixedPeriod
  | Arbitrary FixedPeriods
  | OnceOff
  deriving (Eq, Ord)


instance FixedIntervalL PartialLens Interval where
  fixedIntervalL =
    PLens $ \p -> case p of
      Fixed n -> Just (store fixedInterval n)
      _ -> Nothing

instance ArbitraryIntervalL PartialLens Interval where
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
