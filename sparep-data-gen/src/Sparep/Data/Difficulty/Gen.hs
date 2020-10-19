module Sparep.Data.Difficulty.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString
import Data.GenValidity.Text
import Sparep.Data.Difficulty

instance GenValid Difficulty where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
