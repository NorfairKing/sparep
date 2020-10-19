{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.Data.Repetition.Gen where

import Data.GenValidity
import Data.GenValidity.Time ()
import Sparep.Data.CardId.Gen ()
import Sparep.Data.Difficulty.Gen ()
import Sparep.Data.Repetition

instance GenValid Repetition where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
