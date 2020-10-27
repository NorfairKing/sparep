{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.Data.StudyUnit.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Sparep.Data.Card.Gen ()
import Sparep.Data.FillExercise.Gen ()
import Sparep.Data.StudyUnit

instance GenValid StudyUnit where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
