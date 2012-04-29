module Math.Budget.Priors where

import Math.Budget.Money
import Math.Budget.FixedPeriod
import Data.Time
import Data.List.NonEmpty

data Priors =
  Arbitrary (NonEmpty (Money, DiffTime))
  | Fixed (NonEmpty Money) FixedPeriod
