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
import Math.Budget.PaymentInterval
import Math.Budget.Types
import Math.Budget.Lens.MethodL
import Math.Budget.Lens.PaymentIntervalL
import Math.Budget.Lens.PaymentNameL
import Math.Budget.Lens.PaymentTypeL
import Math.Budget.Lens.ZonedTimeL
import Math.Budget.Lens.AssociationsL
import Data.Lens.Common
import Control.Comonad.Trans.Store
import qualified Data.Set as S
import Data.Time

data Payment =
  Payment String PaymentInterval ZonedTime Method PaymentType Associations

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

instance PaymentNameL Lens Payment where
  paymentNameL =
    Lens $ \(Payment a b c d e f) ->
      store (\a' -> Payment a' b c d e f) a

instance PaymentIntervalL Lens Payment where
  paymentIntervalL =
    Lens $ \(Payment a b c d e f) ->
      store (\b' -> Payment a b' c d e f) b

instance ZonedTimeL Lens Payment where
  zonedTimeL =
    Lens $ \(Payment a b c d e f) ->
      store (\c' -> Payment a b c' d e f) c

instance MethodL Lens Payment where
  methodL =
    Lens $ \(Payment a b c d e f) ->
      store (\d' -> Payment a b c d' e f) d

instance PaymentTypeL Lens Payment where
  paymentTypeL =
    Lens $ \(Payment a b c d e f) ->
      store (\e' -> Payment a b c d e' f) e

instance AssociationsL Lens Payment where
  associationsL =
    Lens $ \(Payment a b c d e f) ->
      store (\f' -> Payment a b c d e f') f

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