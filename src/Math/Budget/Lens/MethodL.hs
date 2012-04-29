{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.MethodL where

import Math.Budget.Method

class MethodL cat target | target -> cat where
  methodL :: cat target Method
