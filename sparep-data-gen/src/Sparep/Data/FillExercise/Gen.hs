{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.Data.FillExercise.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Sparep.Data.FillExercise
import Sparep.Data.Instructions.Gen ()

instance GenValid FillExercise where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid FillExercisePart where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
