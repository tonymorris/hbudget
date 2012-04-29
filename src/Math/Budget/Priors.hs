module Math.Budget.Priors where

import Math.Budget.Money
import Math.Budget.FixedPeriod
import Data.Time

data Priors =
  PeriodPrior Money ZonedTime [(Money, DiffTime)]
  | FixedPeriodPrior Money [Money] FixedPeriod
