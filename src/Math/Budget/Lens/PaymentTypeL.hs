{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.PaymentTypeL where

import Math.Budget.PaymentType

class PaymentTypeL cat target | target -> cat where
  paymentTypeL :: cat target PaymentType
