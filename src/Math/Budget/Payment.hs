module Math.Budget.Payment
(
  Payment
, payment
, expensePayment
, incomePayment
) where

import Math.Budget.Method
import Math.Budget.PaymentType
import Math.Budget.PaymentInterval
import Data.Time
import qualified Data.Set as S

data Payment =
  Payment String PaymentInterval ZonedTime Method PaymentType (S.Set String)

instance Eq Payment where
  Payment a1 b1 c1 d1 e1 f1 == Payment a2 b2 c2 d2 e2 f2 =
    and [
          a1 == a2
        , b1 == b2
        , zonedTimeToLocalTime c1 == zonedTimeToLocalTime c2
        , zonedTimeZone c1 == zonedTimeZone c2
        , d1 == d2
        , e1 == e2
        , f1 == f2
        ]

payment ::
  String
  -> PaymentInterval
  -> ZonedTime
  -> Method
  -> PaymentType
  -> Payment
payment n i t m y =
  Payment n i t m y S.empty

expensePayment ::
  String
  -> PaymentInterval
  -> ZonedTime
  -> Method
  -> Payment
expensePayment n i t m =
  payment n i t m expense

incomePayment ::
  String
  -> PaymentInterval
  -> ZonedTime
  -> Method
  -> Payment
incomePayment n i t m =
  payment n i t m income