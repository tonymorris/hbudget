{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.PaymentNameL where

class PaymentNameL cat target | target -> cat where
  paymentNameL :: cat target String
