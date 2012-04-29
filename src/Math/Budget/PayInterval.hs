{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Budget.PayInterval
(
  PayInterval
, knownInterval
, meanPriors
, medianPriors
) where

import Math.Budget.Interval
import Math.Budget.Money
import Math.Budget.Priors
import Math.Budget.Lens.KnownIntervalL
import Math.Budget.Lens.MeanPriorsL
import Math.Budget.Lens.MedianPriorsL
import Data.Lens.Partial.Common
import Control.Comonad.Trans.Store

data PayInterval =
  KnownInterval Interval Money
  | MeanPriors Priors
  | MedianPriors Priors
  deriving (Eq, Ord, Show)

instance KnownIntervalL PartialLens PayInterval where
  knownIntervalL =
    PLens $ \p -> case p of
      KnownInterval i n -> Just (store (uncurry knownInterval) (i, n))
      _ -> Nothing

instance MeanPriorsL PartialLens PayInterval where
  meanPriorsL =
    PLens $ \p -> case p of
      MeanPriors n -> Just (store meanPriors n)
      _ -> Nothing

instance MedianPriorsL PartialLens PayInterval where
  medianPriorsL =
    PLens $ \p -> case p of
      MedianPriors n -> Just (store medianPriors n)
      _ -> Nothing

knownInterval ::
  Interval
  -> Money
  -> PayInterval
knownInterval =
  KnownInterval

meanPriors ::
  Priors
  -> PayInterval
meanPriors =
  MeanPriors

medianPriors ::
  Priors
  -> PayInterval
medianPriors =
  MedianPriors

