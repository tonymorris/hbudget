{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.Budget.Lens.InternetMethodL where

import Network.URI

class InternetMethodL cat target | target -> cat where
  internetMethodL :: cat target URI
