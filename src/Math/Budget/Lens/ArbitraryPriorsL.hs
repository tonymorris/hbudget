{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.ArbitraryPriorsL where

import Math.Budget.Money
import Math.Budget.FixedPeriod
import Data.List.NonEmpty

class ArbitraryPriorsL cat target | target -> cat where
  arbitraryPriorsL :: cat target (NonEmpty (Money, FixedPeriod))
