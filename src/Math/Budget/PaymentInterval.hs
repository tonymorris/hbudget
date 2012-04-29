{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Budget.PaymentInterval
(
  PaymentInterval
, knownInterval
, functionOfPriors
) where

import Math.Budget.Interval
import Math.Budget.Money
import Math.Budget.Priors
import Math.Budget.Lens.KnownIntervalL
import Math.Budget.Lens.FunctionOfPriorsL
import Data.Lens.Partial.Common
import Control.Comonad.Trans.Store

data PaymentInterval =
  KnownInterval Interval Money
  | FunctionOfPriors Priors
  deriving (Eq, Ord, Show)

instance KnownIntervalL PartialLens PaymentInterval where
  knownIntervalL =
    PLens $ \p -> case p of
      KnownInterval i n -> Just (store (uncurry knownInterval) (i, n))
      _ -> Nothing

instance FunctionOfPriorsL PartialLens PaymentInterval where
  functionOfPriorsL =
    PLens $ \p -> case p of
      FunctionOfPriors n -> Just (store functionOfPriors n)
      _ -> Nothing

knownInterval ::
  Interval
  -> Money
  -> PaymentInterval
knownInterval =
  KnownInterval

functionOfPriors ::
  Priors
  -> PaymentInterval
functionOfPriors =
  FunctionOfPriors

