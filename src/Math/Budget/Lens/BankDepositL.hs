{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.BankDepositL where

import Math.Budget.Types

class BankDepositL cat target | target -> cat where
  bankDepositL :: cat target (AccountName, BSB, AccountNumber)
