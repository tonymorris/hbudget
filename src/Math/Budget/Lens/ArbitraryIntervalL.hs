{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.ArbitraryIntervalL where

import Math.Budget.FixedPeriod

class ArbitraryIntervalL cat target | target -> cat where
  arbitraryIntervalL :: cat target FixedPeriods
