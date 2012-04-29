{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.EveryMonthsL where

class EveryMonthsL cat target | target -> cat where
  everyMonthsL :: cat target Int
