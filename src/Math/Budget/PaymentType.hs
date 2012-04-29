module Math.Budget.PaymentType
(
  PaymentType
, expense
, income
, isExpense
, isIncome
) where

newtype PaymentType =
  PaymentType Bool

expense ::
  PaymentType
expense =
  PaymentType True

income ::
  PaymentType
income =
  PaymentType False

isExpense ::
  PaymentType
  -> Bool
isExpense (PaymentType p) =
  p

isIncome ::
  PaymentType
  -> Bool
isIncome =
  not . isExpense