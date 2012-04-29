{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.FunctionOfPriorsL where

import Math.Budget.Priors

class FunctionOfPriorsL cat target | target -> cat where
  functionOfPriorsL :: cat target Priors
