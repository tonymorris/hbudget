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
  deriving (Eq, Ord)

instance Show PaymentType where
  show (PaymentType t) =
    if t then "expense" else "income"

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