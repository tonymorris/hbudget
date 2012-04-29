{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.FixedPriorsL where

import Math.Budget.Money
import Math.Budget.FixedPeriod
import Data.List.NonEmpty

class FixedPriorsL cat target | target -> cat where
  fixedPriorsL :: cat target (NonEmpty Money, FixedPeriod)
