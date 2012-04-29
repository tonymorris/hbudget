{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Math.Budget.Money
(
  Money
, showMoney
) where

import Data.Fixed
import Data.Data

newtype Money =
  Money (Fixed E2)
  deriving (Eq, Ord, Show, Real, Fractional, RealFrac, Num, Enum, Data, Typeable)

showMoney ::
  Bool -- ^ Whether to chop off trailing zeros.
  -> Money
  -> String
showMoney p (Money x) =
  showFixed p x
