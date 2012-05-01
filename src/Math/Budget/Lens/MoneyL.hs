{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.MoneyL where

import Math.Budget.Money

class MoneyL cat target | target -> cat where
  moneyL :: cat target Money
