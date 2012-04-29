{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.ArbitraryMethodL where

class ArbitraryMethodL cat target | target -> cat where
  arbitraryMethodL :: cat target String
