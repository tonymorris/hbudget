{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.AssociationsL where

import Math.Budget.Types

class AssociationsL cat target | target -> cat where
  associationsL :: cat target Associations
