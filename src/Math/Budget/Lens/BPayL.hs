{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.BPayL where

import Math.Budget.Types

class BPayL cat target | target -> cat where
  bpayL :: cat target (BillerCode, AccountNumber)
