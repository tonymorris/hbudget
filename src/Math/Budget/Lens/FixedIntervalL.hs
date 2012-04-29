{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.FixedIntervalL where

import Math.Budget.FixedPeriod

class FixedIntervalL cat target | target -> cat where
  fixedIntervalL :: cat target FixedPeriod
