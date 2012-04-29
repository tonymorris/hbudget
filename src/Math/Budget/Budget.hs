module Math.Budget.Budget
(
  Budget
, budget
, payments
) where

import Math.Budget.Payment

newtype Budget =
  Budget [Payment]
  deriving (Eq, Show)

budget ::
  [Payment]
  -> Budget
budget =
  Budget

payments ::
  Budget
  -> [Payment]
payments (Budget p) =
  p