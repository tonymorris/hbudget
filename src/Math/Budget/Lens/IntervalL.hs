{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.IntervalL where

import Math.Budget.Interval

class IntervalL cat target | target -> cat where
  intervalL :: cat target Interval
