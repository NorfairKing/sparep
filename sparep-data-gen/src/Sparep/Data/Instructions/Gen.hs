{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.Data.Instructions.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Containers ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Sparep.Data.Instructions

instance GenValid Instructions where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
