module Math.Budget.Priors
(
  Priors
, arbitraryPriorsL
, fixedPriorsL
, arbitraryPriors
, fixedPriors
) where

import Math.Budget.Money
import Math.Budget.FixedPeriod
import Data.Time
import Data.List.NonEmpty
import Data.Lens.Partial.Common
import Control.Comonad.Trans.Store

data Priors =
  Arbitrary (NonEmpty (Money, DiffTime))
  | Fixed (NonEmpty Money) FixedPeriod

arbitraryPriorsL ::
  PartialLens Priors (NonEmpty (Money, DiffTime))
arbitraryPriorsL =
  PLens $ \p -> case p of
    Arbitrary n -> Just (store arbitraryPriors n)
    Fixed _ _ -> Nothing

fixedPriorsL ::
  PartialLens Priors (NonEmpty Money, FixedPeriod)
fixedPriorsL =
  PLens $ \p -> case p of
    Fixed n q -> Just (store (uncurry fixedPriors) (n, q))
    Arbitrary _ -> Nothing

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
