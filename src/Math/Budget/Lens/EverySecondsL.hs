{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.EverySecondsL where

class EverySecondsL cat target | target -> cat where
  everySecondsL :: cat target Integer
