{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.Data.Difficulty.Gen where

import Data.GenValidity
import Sparep.Data.Difficulty

instance GenValid Difficulty where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
