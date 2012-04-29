{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.MedianPriorsL where

import Math.Budget.Priors

class MedianPriorsL cat target | target -> cat where
  medianPriorsL :: cat target Priors
