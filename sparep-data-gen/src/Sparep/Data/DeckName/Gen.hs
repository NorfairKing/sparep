{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.Data.DeckName.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Sparep.Data.DeckName

instance GenValid DeckName where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
