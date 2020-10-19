{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.Data.CardId.Gen where

import Data.GenValidity
import Sparep.Data.Card.Gen ()
import Sparep.Data.CardId

instance GenValid CardId where
  genValid = hashCard <$> genValid
  shrinkValid _ = []
