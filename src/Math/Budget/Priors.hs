{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Budget.Priors
(
  Priors
, arbitraryPriors
, fixedPriors
) where

import Math.Budget.Money
import Math.Budget.FixedPeriod
import Math.Budget.Lens.ArbitraryPriorsL
import Math.Budget.Lens.FixedPriorsL
import Math.Budget.Lens.FixedIntervalL
import Math.Budget.Lens.EverySecondsL
import Math.Budget.Lens.EveryMonthsL
import Data.Time
import Data.List.NonEmpty
import Data.Lens.Common
import Data.Lens.Partial.Common
import Control.Category
import Control.Comonad.Trans.Store

data Priors =
  Arbitrary (NonEmpty (Money, DiffTime))
  | Fixed (NonEmpty Money) FixedPeriod
  deriving (Eq, Ord, Show)

instance ArbitraryPriorsL PartialLens Priors where
  arbitraryPriorsL =
    PLens $ \p -> case p of
      Arbitrary n -> Just (store arbitraryPriors n)
      Fixed _ _ -> Nothing

instance FixedPriorsL PartialLens Priors where
  fixedPriorsL =
    PLens $ \p -> case p of
      Fixed n q -> Just (store (uncurry fixedPriors) (n, q))
      Arbitrary _ -> Nothing

instance FixedIntervalL PartialLens Priors where
  fixedIntervalL =
    fixedPriorsL >>> totalLens sndLens

instance EverySecondsL PartialLens Priors where
  everySecondsL =
    fixedIntervalL >>> everySecondsL

instance EveryMonthsL PartialLens Priors where
  everyMonthsL =
    fixedIntervalL >>> everyMonthsL

arbitraryPriors ::
  NonEmpty (Money, DiffTime)
  -> Priors
arbitraryPriors =
  Arbitrary

fixedPriors ::
  NonEmpty Money
  -> FixedPeriod
  -> Priors
fixedPriors =
  Fixed
