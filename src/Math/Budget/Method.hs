module Math.Budget.Method where

import Network.URI

type AccountName =
  String

type BSB =
  String

type AccountNumber =
  String

type BillerCode =
  String

data Method =
  Internet URI
  | BankDeposit AccountName BSB AccountNumber
  | BPay BillerCode AccountNumber
  | ArbitraryMethod String
  | Cash
  | Unknown
  deriving Eq
