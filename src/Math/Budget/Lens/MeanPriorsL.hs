{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.MeanPriorsL where

import Math.Budget.Priors

class MeanPriorsL cat target | target -> cat where
  meanPriorsL :: cat target Priors
