{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.PaymentIntervalL where

import Math.Budget.PaymentInterval

class PaymentIntervalL cat target | target -> cat where
  paymentIntervalL :: cat target PaymentInterval
