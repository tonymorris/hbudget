{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Budget.Payment
(
  Payment
, payment
, expensePayment
, incomePayment
) where

import Math.Budget.Method
import Math.Budget.PaymentType
import Math.Budget.Interval
import Math.Budget.Money
import Math.Budget.Priors
import Math.Budget.Types
import Math.Budget.Lens.MethodL
import Math.Budget.Lens.PaymentNameL
import Math.Budget.Lens.PaymentTypeL
import Math.Budget.Lens.ZonedTimeL
import Math.Budget.Lens.AssociationsL
import Math.Budget.Lens.ArbitraryMethodL
import Math.Budget.Lens.BankDepositL
import Math.Budget.Lens.BPayL
import Math.Budget.Lens.InternetMethodL
import Math.Budget.Lens.MoneyL
import Math.Budget.Lens.IntervalL
import Math.Budget.Lens.PriorsL
import Data.Lens.Common
import Data.Lens.Partial.Common
import Control.Category
import Control.Comonad.Trans.Store
import qualified Data.Set as S
import Data.Time

data Payment =
  Payment String ZonedTime Method PaymentType Interval Money Priors Associations
  deriving Show

instance Eq Payment where
  Payment a1 b1 c1 d1 e1 f1 g1 h1 == Payment a2 b2 c2 d2 e2 f2 g2 h2 =
    and [
          a1 == a2
        , zonedTimeToLocalTime b1 == zonedTimeToLocalTime b2
        , c1 == c2
        , d1 == d2
        , e1 == e2
        , f1 == f2
        , g1 == g2
        , h1 == h2
        ]

instance PaymentNameL Lens Payment where
  paymentNameL =
    Lens $ \(Payment a b c d e f g h) ->
      store (\a' -> Payment a' b c d e f g h) a

instance ZonedTimeL Lens Payment where
  zonedTimeL =
    Lens $ \(Payment a b c d e f g h) ->
      store (\b' -> Payment a b' c d e f g h) b

instance MethodL Lens Payment where
  methodL =
    Lens $ \(Payment a b c d e f g h) ->
      store (\c' -> Payment a b c' d e f g h) c

instance PaymentTypeL Lens Payment where
  paymentTypeL =
    Lens $ \(Payment a b c d e f g h) ->
      store (\d' -> Payment a b c d' e f g h) d

instance IntervalL Lens Payment where
  intervalL =
    Lens $ \(Payment a b c d e f g h) ->
      store (\e' -> Payment a b c d e' f g h) e

instance MoneyL Lens Payment where
  moneyL =
    Lens $ \(Payment a b c d e f g h) ->
      store (\f' -> Payment a b c d e f' g h) f

instance PriorsL Lens Payment where
  priorsL =
    Lens $ \(Payment a b c d e f g h) ->
      store (\g' -> Payment a b c d e f g' h) g

instance AssociationsL Lens Payment where
  associationsL =
    Lens $ \(Payment a b c d e f g h) ->
      store (\h' -> Payment a b c d e f g h') h

instance ArbitraryMethodL PartialLens Payment where
  arbitraryMethodL =
    totalLens methodL >>> arbitraryMethodL

instance BankDepositL PartialLens Payment where
  bankDepositL =
    totalLens methodL >>> bankDepositL

instance BPayL PartialLens Payment where
  bpayL =
    totalLens methodL >>> bpayL

instance InternetMethodL PartialLens Payment where
  internetMethodL =
    totalLens methodL >>> internetMethodL

-- String ZonedTime Method PaymentType Interval Money
payment ::
  String
  -> ZonedTime
  -> Method
  -> PaymentType
  -> Interval
  -> Money
  -> Payment
payment n t m y i x =
  Payment n t m y i x (priors []) S.empty

expensePayment ::
  String
  -> ZonedTime
  -> Method
  -> Interval
  -> Money
  -> Payment
expensePayment n t m =
  payment n t m expense

incomePayment ::
  String
  -> ZonedTime
  -> Method
  -> Interval
  -> Money
  -> Payment
incomePayment n t m =
  payment n t m income
