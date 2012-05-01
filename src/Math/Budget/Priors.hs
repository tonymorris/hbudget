{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Budget.Priors
(
  Priors
, priors
, getPriors
) where

import Math.Budget.Money
import Math.Budget.FixedPeriod
import Math.Budget.Lens.ArbitraryPriorsL
import Math.Budget.Lens.FixedPriorsL
import Math.Budget.Lens.FixedIntervalL
import Math.Budget.Lens.EverySecondsL
import Math.Budget.Lens.EveryMonthsL
import Data.List.NonEmpty
import Data.Lens.Common
import Data.Lens.Partial.Common
import Data.Time
import Data.Function
import Control.Category
import Control.Comonad.Trans.Store

data Priors =
  Priors [(Money, ZonedTime)]
  deriving Show

instance Eq Priors where
  (==) =
    (==) `on` (\(Priors p) -> fmap (\(m, z) -> (m, zonedTimeToLocalTime z)) p)

instance Ord Priors where
  compare =
    compare `on` (\(Priors p) -> fmap (\(m, z) -> (m, zonedTimeToLocalTime z)) p)

priors ::
  [(Money, ZonedTime)]
  -> Priors
priors =
  Priors

getPriors ::
  Priors
  -> [(Money, ZonedTime)]
getPriors (Priors p) =
  p
