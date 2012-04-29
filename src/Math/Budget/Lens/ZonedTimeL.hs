{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.ZonedTimeL where

import Data.Time

class ZonedTimeL cat target | target -> cat where
  zonedTimeL :: cat target ZonedTime
