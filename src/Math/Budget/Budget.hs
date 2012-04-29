module Math.Budget.Budget
(
  Budget
, budget
, payments
) where

import Math.Budget.Payment

newtype Budget =
  Budget [Payment]
  deriving Eq

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