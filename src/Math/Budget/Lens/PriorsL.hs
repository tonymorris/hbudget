{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.PriorsL where

import Math.Budget.Priors

class PriorsL cat target | target -> cat where
  priorsL :: cat target Priors
