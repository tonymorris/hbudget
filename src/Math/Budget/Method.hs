{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Budget.Method(
  Method
, isCash
, isUnknown
, internetMethod
, bankDeposit
, bpay
, arbitraryMethod
, cashMethod
, unknownMethod
, module Network.URI
) where

import Network.URI
import Math.Budget.Types
import Math.Budget.Lens.ArbitraryMethodL
import Math.Budget.Lens.BankDepositL
import Math.Budget.Lens.BPayL
import Math.Budget.Lens.InternetMethodL
import Data.Lens.Partial.Common
import Control.Comonad.Trans.Store

data Method =
  Internet URI
  | BankDeposit AccountName BSB AccountNumber
  | BPay BillerCode AccountNumber
  | Arbitrary String
  | Cash
  | Unknown
  deriving (Eq, Show)

instance ArbitraryMethodL PartialLens Method where
  arbitraryMethodL =
    PLens $ \p -> case p of
      Arbitrary n -> Just (store arbitraryMethod n)
      _ -> Nothing

instance BankDepositL PartialLens Method where
  bankDepositL =
    PLens $ \p -> case p of
      BankDeposit n b m -> Just (store (\(n', b', m') -> bankDeposit n' b' m') (n, b, m))
      _ -> Nothing

instance BPayL PartialLens Method where
  bpayL =
    PLens $ \p -> case p of
      BPay c n -> Just (store (uncurry bpay) (c, n))
      _ -> Nothing

instance InternetMethodL PartialLens Method where
  internetMethodL =
    PLens $ \p -> case p of
      Internet u -> Just (store internetMethod u)
      _ -> Nothing

isCash ::
  Method
  -> Bool
isCash =
  (== Cash)

isUnknown ::
  Method
  -> Bool
isUnknown =
  (== Unknown)

internetMethod ::
  URI
  -> Method
internetMethod =
  Internet

bankDeposit ::
  AccountName
  -> BSB
  -> AccountNumber
  -> Method
bankDeposit =
  BankDeposit

bpay ::
  BillerCode
  -> AccountNumber
  -> Method
bpay =
  BPay

arbitraryMethod ::
  String
  -> Method
arbitraryMethod =
  Arbitrary

cashMethod ::
  Method
cashMethod =
  Cash

unknownMethod ::
  Method
unknownMethod =
  Unknown
