{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.KnownIntervalL where

import Math.Budget.Interval
import Math.Budget.Money

class KnownIntervalL cat target | target -> cat where
  knownIntervalL :: cat target (Interval, Money)
